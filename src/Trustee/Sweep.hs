{-# LANGUAGE DeriveGeneric #-}
module Trustee.Sweep (cmdBoundsSweep) where

import Peura

import Control.Arrow                          (returnA, (+++), (>>>), (|||))
import Control.Concurrent.STM                 (STM, orElse)
import Data.Function                          (on)
import Data.List                              (intercalate)
import Data.Semigroup                         (Semigroup (..))
import Data.Traversable                       (for)
import Distribution.Package                   (PackageName)
import Distribution.PackageDescription        (GenericPackageDescription (..))
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.Pretty                    (prettyShow)
import Distribution.Version
       (Version, intersectVersionRanges, orEarlierVersion, orLaterVersion,
        thisVersion, withinRange)
import GHC.Generics                           (Generic)
import Prelude                                (userError)
import System.Exit                            (ExitCode (..))
import System.Path                            (Absolute, Path)

import qualified Data.List.NonEmpty              as NE
import qualified Data.Map.Strict                 as Map
import qualified Data.Set                        as Set

import Trustee.GHC     hiding (index)
import Trustee.Index
import Trustee.Monad
import Trustee.Options
import Trustee.Table
import Trustee.Txt
import Trustee.Depends

import Urakka

-- We'd like to have Foldable1?
import Prelude (maximum, minimum)

-------------------------------------------------------------------------------
-- Sweep
-------------------------------------------------------------------------------

data SweepResult
    = SweepResultPending
    | SweepResultNoPlan
    | SweepResultOk      (NonEmpty Version)
    | SweepResultDepFail (NonEmpty Version)
    | SweepResultFail    (NonEmpty Version)
  deriving (Show, Generic)

instance NFData SweepResult

instance Semigroup SweepResult where
    x <> SweepResultPending = x
    SweepResultPending <> y = y
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
    SweepResultPending    -> mkTxt Black   "..."
    SweepResultNoPlan     -> mkTxt Blue    "no-plan"
    SweepResultOk vs      -> mkTxt Green   (displayVs vs)
    SweepResultDepFail vs -> mkTxt Magenta (displayVs vs)
    SweepResultFail vs    -> mkTxt Red     (displayVs vs)
  where
    displayVs vs
      | length vs > 3 =
            prettyShow (minimum vs) ++ " .. " ++ prettyShow (maximum vs)
            ++ " (" ++ show (length vs) ++ ")"
      | otherwise = intercalate ", " $ map prettyShow $ NE.toList vs


cmdBoundsSweep :: TracerPeu Env Void -> GlobalOpts -> Path Absolute -> Verify -> M (STM String, Urakka () ())
cmdBoundsSweep tracer opts dir verify = do
    let ghcs = ghcsInRange (goGhcVersions opts)
    xs <- globDir1 "*.cabal" dir

    case xs of
        [cabalFile] -> do
            putInfo tracer "Reading hackage index"
            index' <- liftIO $ readIndex (goIndexState opts)
            let index = indexValueVersions (goIncludeDeprecated opts) <$> index'

            putInfo tracer "Reading cabal file"
            gpd <- liftIO $ readGenericPackageDescription maxBound (toFilePath cabalFile)

            colsR <- for ghcs $ \ghcVer -> do
                cells <- sweepForGhc verify dir index gpd ghcVer
                let addVersion :: Map PackageName a -> Map (GHCVer, PackageName) a
                    addVersion = Map.mapKeys ((,) ghcVer)
                return $ bimap addVersion (fmap addVersion) cells

            let stms :: STM (Map (GHCVer, PackageName) SweepResult)
                stms = traverse (`orElse` return SweepResultPending)
                    $ Map.unions $ map fst colsR

                cols :: Urakka () (Map (GHCVer, PackageName) SweepResult)
                cols = Map.unions <$> traverse snd colsR

            out <- urakka cols $ \cols' ->
                putStrs [ renderTable $ makeTable makeSweepCell ghcs cols' ]

            return (renderTable . makeTable makeSweepCell ghcs <$> stms, out)

        _ -> throwM (userError "no .cabal file found")

sweepForGhc
    :: Verify
    -> Path Absolute
    -> Map.Map PackageName (Set.Set Version)
    -> GenericPackageDescription
    -> GHCVer
    -> M (Map.Map PackageName (STM SweepResult), Urakka () (Map.Map PackageName SweepResult))
sweepForGhc verify dir index gpd ghcVer = do
    let deps = allBuildDepends (toVersion ghcVer) gpd
    let deps' = Map.intersectionWith withinRange' index deps

    -- putStrs $ map (uncurry formatDep) $ Map.toList deps'

    resR <- for (Map.toList deps') $ \(pn, vs) -> do
        let f :: a -> (PackageName, a)
            f = (,) pn
        fmap (bimap f (fmap f)) $ sweep pn vs

    let stms :: Map PackageName (STM SweepResult)
        stms = Map.fromList $ map fst resR

        res :: Urakka () (Map PackageName SweepResult)
        res = Map.fromList <$> traverse snd resR

    return (stms, res)
  where
    withinRange' vs vr = filter (`withinRange` vr) $ Set.toList vs

    -- formatDep :: PackageName -> [Version] -> String
    -- formatDep pn vs = prettyShow pn <> " : " <> intercalate ", " (map prettyShow vs)

    sweep :: PackageName -> [Version] -> M (STM SweepResult, Urakka () SweepResult)
    sweep pn vs = mconcat <$> for majorVs (resultRange pn)
      where
        majorVs :: [NonEmpty Version]
        majorVs = NE.groupBy ((==) `on` extractMajorVersion) vs

    resultRange :: PackageName -> NonEmpty Version -> M (STM SweepResult, Urakka () SweepResult)
    resultRange pn r = do
        anyOk <- urakka (pure ()) $ \() -> do
            (ec, _, _) <- runCabal ModeDry dir ghcVer $ Map.singleton pn $
                intersectVersionRanges (orLaterVersion $ minimum r) (orEarlierVersion $ maximum r)
            return $ case ec of
                ExitSuccess   -> True
                ExitFailure _ -> False

        ur <- if_ anyOk <$> (sconcat <$> for r (result pn)) <*> return (pure SweepResultNoPlan)
        (stm, post) <- urakkaSTM return
        return (stm, ur >>> post)

    result :: PackageName -> Version -> M (Urakka () SweepResult)
    result pn v = do
        let constraints  = Map.singleton pn (thisVersion v)
        uDry <- urakka' $ \() -> do
                (ec, _, _) <- runCabal ModeDry dir ghcVer constraints
                return $ case ec of
                    ExitSuccess   -> Right $ SweepResultOk $ pure v
                    ExitFailure _ -> Left ()

        uAll <- case verify of
            SolveOnly -> return uDry

            Verify ->  do
                uDep <- urakka' $ \res -> do
                    (ec, _, _) <- runCabal ModeDep dir ghcVer constraints
                    if isFailure ec
                    then return $ Left (SweepResultDepFail $ pure v)
                    else return $ Right res

                uBld <- urakka' $ \res -> do
                    (ec, _, _) <- runCabal ModeBuild dir ghcVer constraints
                    if isFailure ec
                    then return $ SweepResultFail $ pure v
                    else return res

                return $ uDry >>> returnA +++ (uDep >>> returnA ||| uBld)

        return $ uAll >>> pure SweepResultNoPlan ||| returnA

    isFailure (ExitFailure _) = True
    isFailure ExitSuccess     = False

instance Semigroup a => Semigroup (STM a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (STM a) where
    mempty = pure mempty

-------------------------------------------------------------------------------
-- Table
-------------------------------------------------------------------------------

makeTable :: (a -> Txt) -> [GHCVer] -> Map.Map (GHCVer, PackageName) a -> [[Txt]]
makeTable mkCell ghcs m
    = (emptyTxt : map (mkTxt Black . prettyShow. toVersion) ghcs)
    : map mkRow pns
  where
    pns = Set.toList $ Set.map snd $ Map.keysSet m

    mkRow :: PackageName -> [Txt]
    mkRow pn
        = mkTxt Black (prettyShow pn)
        : map (\g -> maybe emptyTxt mkCell $ Map.lookup (g, pn) m) ghcs
