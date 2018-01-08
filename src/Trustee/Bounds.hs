module Trustee.Bounds (cmdBounds) where

import Control.Monad                         (when)
import Control.Monad.IO.Class                (liftIO)
import Distribution.Compiler                 (CompilerFlavor (..))
import Distribution.Package                  (PackageName)
import Distribution.PackageDescription       (GenericPackageDescription (..))
import Distribution.PackageDescription.Parse (readGenericPackageDescription)
import Distribution.System                   (OS (..))
import Distribution.Text                     (display)
import Distribution.Types.CondTree           (simplifyCondTree)
import Distribution.Types.Dependency         (Dependency (..))
import Distribution.Version
       (Version, VersionRange, intersectVersionRanges, simplifyVersionRange,
       versionNumbers, withinRange)
import Path                                  (Abs, Dir, Path)
import System.Exit                           (ExitCode (..))
import System.FilePath.Glob                  (compile, globDir1)

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

cmdBounds :: GlobalOpts -> Path Abs Dir -> Bool -> Limit -> M ()
cmdBounds opts dir verify limit = do
    let ghcs = ghcsInRange (goGhcVersions opts)
    (jGHC, jCabal) <- jobs
    xs <- liftIO $ globDir1 (compile "*.cabal") (Path.toFilePath dir)

    case xs of
        [cabalFile] -> do
            index <- liftIO readIndex
            gpd <- liftIO $ readGenericPackageDescription maxBound cabalFile
            cols <- fmap Map.unions $ forConcurrently ghcs $ \ghcVersion -> do
                cells <- boundsForGhc verify jGHC jCabal limit dir index gpd ghcVersion
                return $ Map.mapKeysMonotonic ((,) ghcVersion) cells

            putStrs [ renderTable $ makeTable ghcs cols ]

        _ -> fail "no .cabal file found"

makeTable :: [GHCVer] -> Map.Map (GHCVer, PackageName) Result -> [[Txt]]
makeTable ghcs m
    = (emptyTxt : map (mkTxt Black . display. toVersion) ghcs)
    : map mkRow pkgNames
  where
    pkgNames = Set.toList $ Set.map snd $ Map.keysSet m

    mkRow :: PackageName -> [Txt]
    mkRow pkgName
        = mkTxt Black (display pkgName)
        : map (\g -> maybe emptyTxt mkCell $ Map.lookup (g, pkgName) m) ghcs

data Result
    = ResultOk      !Version
    | ResultAlmost  !Version
    | ResultNoPlan
    | ResultDepFail !Version
    | ResultFail    !Version
  deriving Show

mkCell ::  Result -> Txt
mkCell (ResultOk v)      = mkTxt Green   $ display v
mkCell (ResultAlmost v)  = mkTxt Cyan    $ display v
mkCell ResultNoPlan      = mkTxt Blue    "no-plan"
mkCell (ResultDepFail v) = mkTxt Magenta $ display v
mkCell (ResultFail v)    = mkTxt Red     $ display v

boundsForGhc
    :: Bool
    -> Int -> Int
    -> Limit
    -> Path Abs Dir
    -> Index
    -> GenericPackageDescription
    -> GHCVer
    -> M (Map.Map PackageName Result)
boundsForGhc verify _jGHC jCabal limit dir index gpd ghcVersion = do
    let deps = allBuildDepends (toVersion ghcVersion) gpd
    let deps' = Map.intersectionWith withinRange' index deps

    fmap Map.fromList $ forConcurrently (Map.toList deps') $ \(pkgName, vs) ->
        fmap ((,) pkgName) $ findLowest pkgName Nothing $ case limit of
            LimitLower -> vs
            LimitUpper -> reverse vs
  where
    withinRange' vs vr = filter (`withinRange` vr) $ Set.toList vs

    findLowest :: PackageName -> Maybe Version -> [Version] -> M Result
    findLowest _       _ []       = return ResultNoPlan
    findLowest pkgName u (v : vs) = do
        (ec, _, _) <- runWithGHC True dir ghcVersion "cabal"
            [ "new-build"
            , "--builddir=.dist-newstyle-" ++ display (toVersion ghcVersion) ++ "-" ++ display pkgName ++ "-" ++ display v
            , "-w", "ghc-" ++ display (toVersion ghcVersion)
            , "--disable-tests", "--disable-benchmarks"
            , "-j" ++ show jCabal
            , "--constraint=" ++ display pkgName ++ "==" ++ display v
            -- , "--ghc-options=" ++ ghcOptions
            , "--dry-run"
            , "all"
            ]

        case ec of
            ExitSuccess   -> doVerify pkgName u v
            ExitFailure _ -> findLowest pkgName (Just v) vs

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
            (ec, _, _) <- lift $ runWithGHC False dir ghcVersion "cabal"
                [ "new-build"
                , "--builddir=.dist-newstyle-" ++ display (toVersion ghcVersion) ++ "-" ++ display pkgName ++ "-" ++ display v
                , "-w", "ghc-" ++ display (toVersion ghcVersion)
                , "--disable-tests", "--disable-benchmarks"
                , "-j" ++ show jCabal
                , "--constraint=" ++ display pkgName ++ "==" ++ display v
                -- , "--ghc-options=" ++ ghcOptions
                , "all"
                , "--dependencies"
                ]

            when (isFailure ec) $ exit $ ResultDepFail v

            (ec', _, _) <- lift $ runWithGHC False dir ghcVersion "cabal"
                [ "new-build"
                , "--builddir=.dist-newstyle-" ++ display (toVersion ghcVersion) ++ "-" ++ display pkgName ++ "-" ++ display v
                , "-w", "ghc-" ++ display (toVersion ghcVersion)
                , "--disable-tests", "--disable-benchmarks"
                , "-j" ++ show jCabal
                , "--constraint=" ++ display pkgName ++ "==" ++ display v
                -- , "--ghc-options=" ++ ghcOptions
                , "all"
                ]

            when (isFailure ec') $ exit $ ResultFail v

            return $ resultOk u v

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
