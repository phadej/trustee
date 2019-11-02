{-# LANGUAGE DeriveGeneric #-}
module Trustee.Sweep (cmdSweep) where

import Algebra.Lattice
       (BoundedMeetSemiLattice (top), Lattice (..), meets)
import Control.Arrow                          (returnA, (>>>), (|||), (+++))
import Control.DeepSeq                        (NFData)
import Control.Monad                          (unless, when)
import Control.Monad.IO.Class                 (liftIO)
import Data.Function                          (on)
import Data.List                              (intercalate)
import Data.List.NonEmpty                     (NonEmpty (..))
import Data.Maybe                             (listToMaybe)
import Data.Semigroup
       (Max (..), Min (..), Option (..), Semigroup (..), option)
import Data.Traversable                       (for)
import Distribution.Compiler                  (CompilerFlavor (..))
import Distribution.Package                   (PackageName)
import Distribution.PackageDescription        (GenericPackageDescription (..))
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.Pretty                    (prettyShow)
import Distribution.System                    (OS (..))
import Distribution.Types.Condition           (simplifyCondition)
import Distribution.Types.CondTree
       (CondBranch (..), CondTree (..), mapTreeConstrs)
import Distribution.Types.Dependency          (Dependency (..))
import Distribution.Version
       (Version, VersionRange, intersectVersionRanges, mkVersion,
       orEarlierVersion, orLaterVersion, simplifyVersionRange, thisVersion,
       unionVersionRanges, versionNumbers, withinRange)
import GHC.Generics                           (Generic)
import System.Exit                            (ExitCode (..))
import System.FilePath.Glob                   (compile, globDir1)
import System.Path                            (Absolute, Path)

import qualified Data.List.NonEmpty              as NE
import qualified Data.Map.Strict                 as Map
import qualified Data.Set                        as Set
import qualified Distribution.PackageDescription as PD
import qualified System.Path                     as Path

import Trustee.GHC     hiding (index)
import Trustee.Index
import Trustee.Monad
import Trustee.Options
import Trustee.Table
import Trustee.Txt
import Trustee.Util

import Urakka
-------------------------------------------------------------------------------
-- Sweep
-------------------------------------------------------------------------------

data SweepResult
    = SweepResultNoPlan
    | SweepResultOk      (NonEmpty Version)
    | SweepResultDepFail (NonEmpty Version)
    | SweepResultFail    (NonEmpty Version)
  deriving (Show, Generic)

instance NFData SweepResult

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
            prettyShow (minimum vs) ++ " .. " ++ prettyShow (maximum vs)
            ++ " (" ++ show (length vs) ++ ")"
      | otherwise = intercalate ", " $ map prettyShow $ NE.toList vs


cmdBoundsSweep :: GlobalOpts -> Path Absolute -> Bool -> M ()
cmdBoundsSweep opts dir verify = do
    let ghcs = ghcsInRange (goGhcVersions opts)
    xs <- liftIO $ globDir1 (compile "*.cabal") (Path.toFilePath dir)
    case xs of
        [cabalFile] -> do
            index' <- liftIO $ fst <$> readIndex (goIndexState opts)
            let index = indexValueVersions (goIncludeDeprecated opts) <$> index'
            gpd <- liftIO $ readGenericPackageDescription maxBound cabalFile
            cols <- fmap Map.unions $ forConcurrently ghcs $ \ghcVersion -> do
                cells <- sweepForGhc verify dir index gpd ghcVersion
                return $ Map.mapKeysMonotonic ((,) ghcVersion) cells

            putStrs [ renderTable $ makeTable makeSweepCell ghcs cols ]

        _ -> fail "no .cabal file found"

sweepForGhc
    :: Bool
    -> Path Absolute
    -> Map.Map PackageName (Set.Set Version)
    -> GenericPackageDescription
    -> GHCVer
    -> M (Map.Map PackageName SweepResult)
sweepForGhc verify dir index gpd ghcVersion = do
    let deps = allBuildDepends (toVersion ghcVersion) gpd
    let deps' = Map.intersectionWith withinRange' index deps

    -- putStrs $ map (uncurry formatDep) $ Map.toList deps'

    fmap Map.fromList $ forConcurrently (Map.toList deps') $ \(pkgName, vs) ->
        fmap ((,) pkgName) $ sweep pkgName vs
  where
    withinRange' vs vr = filter (`withinRange` vr) $ Set.toList vs

    -- formatDep :: PackageName -> [Version] -> String
    -- formatDep pn vs = prettyShow pn <> " : " <> intercalate ", " (map prettyShow vs)

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
