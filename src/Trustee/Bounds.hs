module Trustee.Bounds (cmdBounds) where

import Control.Monad                         (unless, when)
import Control.Monad.IO.Class                (liftIO)
import Data.Function                         (on)
import Data.List                             (intercalate)
import Data.List.NonEmpty                    (NonEmpty (..))
import Data.Maybe                            (listToMaybe)
import Data.Semigroup
       (Max (..), Min (..), Option (..), Semigroup (..), option)
import Distribution.Compiler                 (CompilerFlavor (..))
import Distribution.Package                  (PackageName)
import Distribution.PackageDescription       (GenericPackageDescription (..))
import Distribution.PackageDescription.Parse (readGenericPackageDescription)
import Distribution.System                   (OS (..))
import Distribution.Text                     (display)
import Distribution.Types.CondTree           (simplifyCondTree)
import Distribution.Types.Dependency         (Dependency (..))
import Distribution.Version
       (Version, VersionRange, intersectVersionRanges, mkVersion,
       orEarlierVersion, orLaterVersion, simplifyVersionRange, thisVersion,
       versionNumbers, withinRange)
import Path                                  (Abs, Dir, Path)
import System.Exit                           (ExitCode (..))
import System.FilePath.Glob                  (compile, globDir1)

import qualified Data.List.NonEmpty              as NE
import qualified Data.Map.Strict                 as Map
import qualified Data.Set                        as Set
import qualified Distribution.PackageDescription as PD
import qualified Path

import Trustee.GHC     hiding (index)
import Trustee.Index
import Trustee.Monad
import Trustee.Options
import Trustee.Table
import Trustee.Txt
import Trustee.Util

cmdBounds :: GlobalOpts -> Path Abs Dir -> Bool -> Maybe Limit -> M ()
cmdBounds opts dir verify (Just limit) = cmdBoundsLimit opts dir verify limit
cmdBounds opts dir verify Nothing      = cmdBoundsSweep opts dir verify

-------------------------------------------------------------------------------
-- Sweep
-------------------------------------------------------------------------------

data SweepResult
    = SweepResultNoPlan
    | SweepResultOk      (NonEmpty Version)
    | SweepResultDepFail (NonEmpty Version)
    | SweepResultFail    (NonEmpty Version)
  deriving Show

instance Semigroup SweepResult where
    x <> SweepResultNoPlan = x
    SweepResultNoPlan <> y = y
    SweepResultFail x <> SweepResultFail y = SweepResultFail (x <> y)
    x@SweepResultFail {} <> _ = x
    _ <> y@SweepResultFail {} = y
    SweepResultDepFail x <> SweepResultDepFail y = SweepResultDepFail (x <> y)
    x@SweepResultDepFail {} <> _ = x
    _ <> y@SweepResultDepFail {} = y
    SweepResultOk x <> SweepResultOk y = SweepResultOk (x <> y)

instance Monoid SweepResult where
    mempty = SweepResultNoPlan
    mappend = (<>)

makeSweepCell :: SweepResult -> Txt
makeSweepCell r = case r of
    SweepResultNoPlan     -> mkTxt Blue    "no-plan"
    SweepResultOk vs      -> mkTxt Green   (displayVs vs)
    SweepResultDepFail vs -> mkTxt Magenta (displayVs vs)
    SweepResultFail vs    -> mkTxt Red     (displayVs vs)
  where
    displayVs vs
      | length vs > 3 =
            display (minimum vs) ++ " .. " ++ display (maximum vs)
            ++ " (" ++ show (length vs) ++ ")"
      | otherwise = intercalate ", " $ map display $ NE.toList vs


cmdBoundsSweep :: GlobalOpts -> Path Abs Dir -> Bool -> M ()
cmdBoundsSweep opts dir verify = do
    let ghcs = ghcsInRange (goGhcVersions opts)
    xs <- liftIO $ globDir1 (compile "*.cabal") (Path.toFilePath dir)
    case xs of
        [cabalFile] -> do
            index <- liftIO $ fst <$> readIndex (goIndexState opts)
            gpd <- liftIO $ readGenericPackageDescription maxBound cabalFile
            cols <- fmap Map.unions $ forConcurrently ghcs $ \ghcVersion -> do
                cells <- sweepForGhc verify dir index gpd ghcVersion
                return $ Map.mapKeysMonotonic ((,) ghcVersion) cells

            putStrs [ renderTable $ makeTable makeSweepCell ghcs cols ]

        _ -> fail "no .cabal file found"

sweepForGhc
    :: Bool
    -> Path Abs Dir
    -> Index
    -> GenericPackageDescription
    -> GHCVer
    -> M (Map.Map PackageName SweepResult)
sweepForGhc verify dir index gpd ghcVersion = do
    let deps = allBuildDepends (toVersion ghcVersion) gpd
    let deps' = Map.intersectionWith withinRange' index deps

    fmap Map.fromList $ forConcurrently (Map.toList deps') $ \(pkgName, vs) ->
        fmap ((,) pkgName) $ sweep pkgName vs
  where
    withinRange' vs vr = filter (`withinRange` vr) $ Set.toList vs

    sweep :: PackageName -> [Version] -> M SweepResult
    sweep pkgName vs = mconcat <$> forConcurrently majorVs (resultRange pkgName)
      where
        majorVs :: [NonEmpty Version]
        majorVs = NE.groupBy ((==) `on` extractMajorVersion) vs

    resultRange :: PackageName -> NonEmpty Version -> M SweepResult
    resultRange pkgName r = do
        (ec, _, _) <- runCabal ModeDry dir ghcVersion $ Map.singleton pkgName $
            intersectVersionRanges (orLaterVersion $ minimum r) (orEarlierVersion $ maximum r)
        case ec of
            ExitFailure _ -> return SweepResultNoPlan
            _             -> sconcat <$> forConcurrently r (result pkgName)

    result :: PackageName -> Version -> M SweepResult
    result pkgName v = runEarlyExit $ do
        let cons' = Map.singleton pkgName (thisVersion v)
        (ec0, _, _) <- lift $ runCabal ModeDry dir ghcVersion cons'
        when (isFailure ec0) $ exit SweepResultNoPlan

        unless verify $ exit $ SweepResultOk $ pure v

        (ec1, _, _) <- lift $ runCabal ModeDep dir ghcVersion cons'
        when (isFailure ec1) $ exit $ SweepResultDepFail $ pure v

        (ec2, _, _) <- lift $ runCabal ModeBuild dir ghcVersion cons'
        when (isFailure ec2) $ exit $ SweepResultFail $ pure v

        return $ SweepResultOk $ pure v

    isFailure (ExitFailure _) = True
    isFailure ExitSuccess     = False

-------------------------------------------------------------------------------
-- Limit
-------------------------------------------------------------------------------

cmdBoundsLimit :: GlobalOpts -> Path Abs Dir -> Bool -> Limit -> M ()
cmdBoundsLimit opts dir verify limit = do
    let ghcs = ghcsInRange (goGhcVersions opts)
    xs <- liftIO $ globDir1 (compile "*.cabal") (Path.toFilePath dir)

    case xs of
        [cabalFile] -> do
            index <- liftIO $ fst <$> readIndex (goIndexState opts)
            gpd <- liftIO $ readGenericPackageDescription maxBound cabalFile
            cols <- fmap Map.unions $ forConcurrently ghcs $ \ghcVersion -> do
                cells <- boundsForGhc verify limit dir index gpd ghcVersion
                return $ Map.mapKeysMonotonic ((,) ghcVersion) cells

            putStrs [ renderTable $ makeTable makeCell ghcs cols ]

        _ -> fail "no .cabal file found"

data Result
    = ResultOk      !Version
    | ResultAlmost  !Version
    | ResultNoPlan
    | ResultDepFail !Version
    | ResultFail    !Version
  deriving Show

makeCell ::  Result -> Txt
makeCell (ResultOk v)      = mkTxt Green   $ display v
makeCell (ResultAlmost v)  = mkTxt Cyan    $ display v
makeCell ResultNoPlan      = mkTxt Blue    "no-plan"
makeCell (ResultDepFail v) = mkTxt Magenta $ display v
makeCell (ResultFail v)    = mkTxt Red     $ display v

boundsForGhc
    :: Bool
    -> Limit
    -> Path Abs Dir
    -> Index
    -> GenericPackageDescription
    -> GHCVer
    -> M (Map.Map PackageName Result)
boundsForGhc verify limit dir index gpd ghcVersion = do
    let deps = allBuildDepends (toVersion ghcVersion) gpd
    let deps' = Map.intersectionWith withinRange' index deps

    fmap Map.fromList $ forConcurrently (Map.toList deps') $ \(pkgName, vs) ->
        fmap ((,) pkgName) $ findLowest pkgName $ case limit of
            LimitLower -> vs
            LimitUpper -> reverse vs
  where
    withinRange' vs vr = filter (`withinRange` vr) $ Set.toList vs

    findLowest :: PackageName -> [Version] -> M Result
    findLowest pkgName vs = divideRanges majorVs
      where
        u = listToMaybe vs

        majorVs :: [NonEmpty Version]
        majorVs = NE.groupBy ((==) `on` extractMajorVersion) vs

        linear :: [Version] -> M Result
        linear [] = return ResultNoPlan
        linear (v:vs') = do
            (ec, _, _) <- runCabal ModeDry dir ghcVersion $ Map.singleton pkgName (thisVersion v)
            case ec of
                ExitSuccess   -> doVerify pkgName u v
                ExitFailure _ -> linear vs'

        linearRanges :: [NonEmpty Version] -> M Result
        linearRanges [] = return ResultNoPlan
        linearRanges [r] = linear (NE.toList r)
        linearRanges (r:rs) = do
            (ec, _, _) <- runCabal ModeDry dir ghcVersion $ Map.singleton pkgName $
                intersectVersionRanges (orLaterVersion $ minimum r) (orEarlierVersion $ maximum r)
            case ec of
                ExitSuccess   -> linear (NE.toList r)
                ExitFailure _ -> linearRanges rs

        divideRanges :: [NonEmpty Version] -> M Result
        divideRanges us
            | length us < 5 = linearRanges us
            | otherwise = do
                (ec, _, _) <- runCabal ModeDry dir ghcVersion $ Map.singleton pkgName $
                    intersectVersionRanges (orLaterVersion $ minimum2 vsl) (orEarlierVersion $ maximum2 vsl)
                case ec of
                    ExitSuccess   -> divideRanges vsl
                    ExitFailure _ -> divideRanges vsr
          where
            (vsl, vsr) = splitAt (length us `div` 2) us

    isFailure (ExitFailure _) = True
    isFailure ExitSuccess     = False

    resultOk :: Maybe Version -> Version -> Result
    resultOk Nothing v  = ResultOk v
    resultOk (Just u) v
        | take 2 (versionNumbers u) == take 2 (versionNumbers v) = ResultOk v
        | otherwise = ResultAlmost v

    doVerify :: PackageName -> Maybe Version -> Version -> M Result
    doVerify pkgName u v | not verify = return $ resultOk u v
        | otherwise = runEarlyExit $ do
            (ec, _, _) <- lift $ runCabal ModeDep dir ghcVersion $ Map.singleton pkgName (thisVersion v)
            when (isFailure ec) $ exit $ ResultDepFail v

            (ec', _, _) <- lift $ runCabal ModeBuild dir ghcVersion $ Map.singleton pkgName (thisVersion v)
            when (isFailure ec') $ exit $ ResultFail v

            return $ resultOk u v

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

allBuildDepends :: Version -> GenericPackageDescription -> Map.Map PackageName VersionRange
allBuildDepends ghcVersion
    = fmap simplifyVersionRange
    . extractBuildDepends
  where
    extractBuildDepends :: GenericPackageDescription -> Map.Map PackageName VersionRange
    extractBuildDepends gpd =
        maybe Map.empty libBD (condLibrary gpd)

    libBD :: PD.CondTree PD.ConfVar [Dependency] PD.Library
          -> Map.Map PackageName VersionRange
    libBD
        = Map.fromListWith intersectVersionRanges
        . map dependencyToPair
        . fst
        . simplifyCondTree (Right . evalConfVar)

    dependencyToPair (Dependency p vr) = (p, vr)

    evalConfVar :: PD.ConfVar -> Bool
    evalConfVar (PD.OS Linux)    = True
    evalConfVar (PD.OS _)        = False
    evalConfVar (PD.Impl GHC vr) = ghcVersion `withinRange` vr
    evalConfVar (PD.Impl _ _)    = False
    evalConfVar _                = True

extractMajorVersion :: Version -> Version
extractMajorVersion v = case versionNumbers v of
    []          -> mkVersion [0]
    [_]         -> v
    [_,_]       -> v
    (x : y : _) -> mkVersion [x, y]

minimum2 :: (Foldable t, Foldable s, Ord a) => t (s a) -> a
minimum2
    = option (error "minimum2: empty case") getMin
    . (foldMap . foldMap) (Option . Just . Min)

maximum2 :: (Foldable t, Foldable s, Ord a) => t (s a) -> a
maximum2
    = option (error "maximum2: empty case") getMax
    . (foldMap . foldMap) (Option . Just . Max)

-------------------------------------------------------------------------------
-- Table
-------------------------------------------------------------------------------

makeTable :: (a -> Txt) -> [GHCVer] -> Map.Map (GHCVer, PackageName) a -> [[Txt]]
makeTable mkCell ghcs m
    = (emptyTxt : map (mkTxt Black . display. toVersion) ghcs)
    : map mkRow pkgNames
  where
    pkgNames = Set.toList $ Set.map snd $ Map.keysSet m

    mkRow :: PackageName -> [Txt]
    mkRow pkgName
        = mkTxt Black (display pkgName)
        : map (\g -> maybe emptyTxt mkCell $ Map.lookup (g, pkgName) m) ghcs
