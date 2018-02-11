module Trustee.NewBuild (
    cmdNewBuild,
    matrixRow,
    matrixRow',
    Result (..),
    ) where

import Control.Applicative  ((<|>))
import Control.Monad        (unless, when)
import Data.Map             (Map)
import Data.Maybe           (mapMaybe)
import Distribution.Package (PackageName, mkPackageName)
import Distribution.Text    (display)
import Distribution.Version
       (VersionRange, intersectVersionRanges, mkVersion, thisVersion)
import Path                 (Abs, Dir, Path)
import System.Exit          (ExitCode (..))

import Trustee.GHC
import Trustee.Monad
import Trustee.Options
import Trustee.Table
import Trustee.Txt
import Trustee.Util

import qualified Cabal.Plan      as Cabal
import qualified Data.Map.Strict as Map
import qualified Data.Text       as T

data Result
    = ResultOk
    | ResultTestFail
    | ResultDryFail ExitCode String String
    | ResultDepFail ExitCode String String
    | ResultFail ExitCode String String
  deriving Show

isResultFail :: Result -> Maybe (String, String)
isResultFail (ResultFail _ o e) = Just (o, e)
isResultFail _                  = Nothing

isResultDepFail :: Result -> Maybe (String, String)
isResultDepFail (ResultDepFail _ o e) = Just (o, e)
isResultDepFail _                     = Nothing

cmdNewBuild :: GlobalOpts -> Path Abs Dir -> [String] -> M ()
cmdNewBuild opts dir args = do
    let ghcs = ghcsInRange (goGhcVersions opts)

    xs <- matrixRow ghcs args dir

    case findMaybe isResultFail xs of
        Just (o, e) -> putStrs
            [ colored Cyan $ "==== STDOUT " ++ replicate 60 '='
            , o
            , colored Red  $ "==== STDERR " ++ replicate 60 '='
            , e
            , colored Cyan $ replicate 72 '='
            ]
        Nothing -> case findMaybe isResultDepFail xs of
            Just (o, e) -> putStrs
                [ ""
                , colored Cyan $ "==== STDOUT " ++ replicate 60 '='
                , ""
                , o
                , ""
                , colored Red  $ "==== STDERR " ++ replicate 60 '='
                , ""
                , e
                , ""
                , colored Cyan $ replicate 72 '='
                , ""
                ]
            Nothing -> pure ()

    putStrs [ renderTable $ reverse $ zipWith mkRow ghcs xs ]
  where
    mkRow :: GHCVer -> Result -> [Txt]
    mkRow v r =
        [ mkTxt Black (display (toVersion v))
        , mkCell r
        ]

    mkCell :: Result -> Txt
    mkCell ResultOk         = mkTxt Green   "success"
    mkCell ResultTestFail   = mkTxt Green   "success, tests failed"
    mkCell ResultDryFail {} = mkTxt Blue    "--dry-run failed"
    mkCell ResultDepFail {} = mkTxt Magenta "--dependencies failed"
    mkCell ResultFail {}    = mkTxt Red     "failure"

matrixRow' :: Traversable t => Bool -> t GHCVer -> Map PackageName VersionRange -> Path Abs Dir -> M (t Result)
matrixRow' test ghcs constraints dir = forConcurrently ghcs $ \ghcVersion -> runEarlyExit $ do
    (ec0, o0, e0) <- lift $ runCabal ModeDry dir ghcVersion constraints
    when (isFailure ec0) $ exit $ ResultDryFail ec0 o0 e0

    (ec1, o1, e1) <- lift $ runCabal ModeDep dir ghcVersion constraints
    when (isFailure ec1) $ exit $ ResultDryFail ec1 o1 e1

    (ec2, o2, e2) <- lift $ runCabal ModeBuild dir ghcVersion constraints
    when (isFailure ec2) $ exit $ ResultFail ec2 o2 e2

    unless test $ exit ResultOk

    plan <- lift $ findPlan dir ghcVersion constraints
    let pkgIds = map Cabal.uPId $ Map.elems (Cabal.pjUnits plan)
    let constraints' = Map.fromListWith intersectVersionRanges $ map mkConstraint pkgIds

    (ec3, _, _) <- lift $ runCabal ModeDryTest dir ghcVersion constraints'
    when (isFailure ec3) $ exit ResultTestFail

    (ec4, _, _) <- lift $ runCabal ModeDepTest dir ghcVersion constraints'
    when (isFailure ec4) $ exit ResultTestFail

    (ec5, _, _) <- lift $ runCabal ModeBuildTest dir ghcVersion constraints'
    when (isFailure ec5) $ exit ResultTestFail

    plan' <- lift $ findPlan dir ghcVersion constraints'
    let comps = concatMap (Map.toList . Cabal.uComps) $ Map.elems (Cabal.pjUnits plan')
    let testBins = mapMaybe testBin comps

    testsFailed <- fmap or $ lift $ forConcurrently testBins $ \exe -> do
        (ec6, _, _) <- runWithGHC ModeBld' dir ghcVersion exe []
        return (isFailure ec6)

    when testsFailed $ exit ResultTestFail

    return ResultOk

  where
    isFailure (ExitFailure _) = True
    isFailure ExitSuccess     = False

    mkConstraint :: Cabal.PkgId -> (PackageName, VersionRange)
    mkConstraint (Cabal.PkgId (Cabal.PkgName pname) (Cabal.Ver vs)) =
        (mkPackageName (T.unpack pname), thisVersion $ mkVersion vs)

    testBin (Cabal.CompNameTest _, ci) = Cabal.ciBinFile ci
    testBin _                          = Nothing

matrixRow :: Traversable t => t GHCVer -> [String] -> Path Abs Dir -> M (t Result)
matrixRow ghcs args dir = do
    (_jGHC, jCabal) <- jobs
    forConcurrently ghcs $ \ghcVersion -> runEarlyExit $ do
        (ec0, o0, e0) <- lift $ runWithGHC ModeDry' dir ghcVersion "cabal" $
            [ "new-build"
            , "-w", "ghc-" ++ display (toVersion ghcVersion)
            , "--builddir=.dist-newstyle-" ++ display (toVersion ghcVersion)
            -- , "--ghc-options=" ++ ghcOptions jGHC ghcVersion
            , "-j" ++ show jCabal
            , "--dry-run"
            ] ++ args

        when (isFailure ec0) $ exit $ ResultDryFail ec0 o0 e0

        (ec1, o1, e1) <- lift $ runWithGHC ModeDep' dir ghcVersion "cabal" $
            [ "new-build"
            , "-w", "ghc-" ++ display (toVersion ghcVersion)
            , "--builddir=.dist-newstyle-" ++ display (toVersion ghcVersion)
            -- , "--ghc-options=" ++ ghcOptions jGHC ghcVersion
            , "--dependencies"
            , "-j" ++ show jCabal
            ] ++ args

        when (isFailure ec1) $ exit $ ResultDepFail ec1 o1 e1

        (ec2, o2, e2) <- lift $ runWithGHC ModeBld' dir ghcVersion "cabal" $
            [ "new-build"
            , "-w", "ghc-" ++ display (toVersion ghcVersion)
            , "--builddir=.dist-newstyle-" ++ display (toVersion ghcVersion)
            -- , "--ghc-options=" ++ ghcOptions jGHC ghcVersion
            , "-j" ++ show jCabal
            ] ++ args

        when (isFailure ec2) $ exit $ ResultFail ec2 o2 e2

        return ResultOk

  where
    isFailure (ExitFailure _) = True
    isFailure ExitSuccess     = False

findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
findMaybe _ [] = Nothing
findMaybe f (x : xs) = f x <|> findMaybe f xs
