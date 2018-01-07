module Trustee.LowerBounds where

import Control.Monad                         (when)
import Control.Monad.IO.Class                (liftIO)
import Data.Maybe                            (mapMaybe)
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
       withinRange)
import Path                                  (Abs, Dir, Path)
import System.Exit                           (ExitCode (..))
import System.FilePath.Glob                  (compile, globDir1)

import qualified Data.Map.Strict                 as Map
import qualified Data.Set                        as Set
import qualified Distribution.PackageDescription as PD
import qualified Path

import Trustee.GHC      hiding (index)
import Trustee.Index
import Trustee.Monad
import Trustee.NewBuild (Result (..))
import Trustee.Options
import Trustee.Table
import Trustee.Txt
import Trustee.Util

cmdLowerBounds :: GlobalOpts -> Path Abs Dir -> Bool -> M ()
cmdLowerBounds opts dir verify = do
    let ghcs = ghcsInRange (goGhcVersions opts)
    (jGHC, jCabal) <- jobs
    xs <- liftIO $ globDir1 (compile "*.cabal") (Path.toFilePath dir)

    case xs of
        [cabalFile] -> do
            index <- liftIO readIndex
            gpd <- liftIO $ readGenericPackageDescription maxBound cabalFile
            cols <- fmap Map.unions $ forConcurrently ghcs $ \ghcVersion -> do
                cells <- lowerBoundsForGhc verify jGHC jCabal dir index gpd ghcVersion
                return $ Map.mapKeysMonotonic ((,) ghcVersion) cells

            putStrs [ renderTable $ makeTable ghcs cols ]

        _ -> fail "no .cabal file found"

makeTable :: [GHCVer] -> Map.Map (GHCVer, PackageName) (Version, Result) -> [[Txt]]
makeTable ghcs m
    = (emptyTxt : map (mkTxt Black . display. toVersion) ghcs)
    : map mkRow pkgNames
  where
    pkgNames = Set.toList $ Set.map snd $ Map.keysSet m

    mkRow :: PackageName -> [Txt]
    mkRow pkgName
        = mkTxt Black (display pkgName)
        : map (\g -> maybe emptyTxt (uncurry mkCell) $ Map.lookup (g, pkgName) m) ghcs

mkCell :: Version -> Result -> Txt
mkCell v ResultOk         = mkTxt Green   $ display v
mkCell v ResultDryFail {} = mkTxt Blue    $ display v
mkCell v ResultDepFail {} = mkTxt Magenta $ display v
mkCell v ResultFail {}    = mkTxt Red     $ display v

lowerBoundsForGhc
    :: Bool
    -> Int -> Int
    -> Path Abs Dir
    -> Index
    -> GenericPackageDescription
    -> GHCVer
    -> M (Map.Map PackageName (Version, Result))
lowerBoundsForGhc verify _jGHC jCabal dir index gpd ghcVersion = do
    let deps = allBuildDepends (toVersion ghcVersion) gpd
    let deps' = Map.intersectionWith withinRange' index deps

    fmap mkMap $ forConcurrently (Map.toList deps') $ \(pkgName, vs) ->
        (,) pkgName <$> findLowest pkgName Nothing vs
  where
    withinRange' vs vr = filter (`withinRange` vr) $ Set.toList vs

    mkMap :: [(PackageName, Maybe (Version, Result))] -> Map.Map PackageName (Version, Result)
    mkMap = Map.fromList . mapMaybe sequence

    findLowest :: PackageName -> Maybe Version -> [Version] -> M (Maybe (Version, Result))
    findLowest _       _ []       = return Nothing
    findLowest pkgName _ (v : vs) = do
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
            ExitSuccess   ->   do
                r <- doVerify pkgName v
                return $ Just (v, r)
            ExitFailure _ -> findLowest pkgName (Just v) vs

    isFailure (ExitFailure _) = True
    isFailure ExitSuccess     = False

    doVerify :: PackageName -> Version -> M Result
    doVerify pkgName v | not verify = return ResultOk
        | otherwise = runEarlyExit $ do
            (ec, o, e) <- lift $ runWithGHC False dir ghcVersion "cabal"
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

            when (isFailure ec) $ exit $ ResultDepFail ec o e

            (ec', o', e') <- lift $ runWithGHC False dir ghcVersion "cabal"
                [ "new-build"
                , "--builddir=.dist-newstyle-" ++ display (toVersion ghcVersion) ++ "-" ++ display pkgName ++ "-" ++ display v
                , "-w", "ghc-" ++ display (toVersion ghcVersion)
                , "--disable-tests", "--disable-benchmarks"
                , "-j" ++ show jCabal
                , "--constraint=" ++ display pkgName ++ "==" ++ display v
                -- , "--ghc-options=" ++ ghcOptions
                , "all"
                ]

            when (isFailure ec') $ exit $ ResultFail ec' o' e'

            return ResultOk

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
