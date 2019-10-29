module Trustee.Matrix2 (cmdMatrix2) where

import Control.Arrow     (arr, returnA, (>>>), (|||))
import Data.Function     (on)
import Data.List         (sortBy)
import Data.Maybe        (mapMaybe)
import Data.Traversable  (for)
import Distribution.Text (display)
import System.Path       (Absolute, Path)

import Distribution.Simple.Utils           (toUTF8BS)
import Distribution.Types.PkgconfigVersion (rpmvercmp)

import Data.Map             (Map)
import Distribution.Package (PackageName)
import Distribution.Version (VersionRange)
import System.Exit          (ExitCode (..))

import qualified System.Path as Path

import Trustee.GHC
import Trustee.Monad
import Trustee.NewBuild (Result (..))
import Trustee.Options
import Trustee.Table
import Trustee.Txt

import Urakka

cmdMatrix2 :: GlobalOpts -> Bool -> [Path Absolute] -> M (Urakka () ())
cmdMatrix2 opts test dirs' = do
    let ghcs = reverse $ ghcsInRange (goGhcVersions opts)
    xssU <- for dirs $ matrixRow' test ghcs mempty

    output <- urakka (sequenceA xssU) $ \xss -> do
        putStrs [ renderTable $ makeTable ghcs xss ]

        -- TODO: some errors
        putStrs $ take 2 $ mapMaybe isFail $ concat xss

    return output
  where
    dirs = sortBy (flip rpmvercmp `on` toUTF8BS . Path.toUnrootedFilePath . Path.takeFileName) dirs'

    makeTable :: [GHCVer] -> [[Result]] -> [[Txt]]
    makeTable ghcs xss
        = (emptyTxt : map (mkTxt Black . display. toVersion) ghcs)
        : zipWith mkRow dirs xss

    mkRow :: Path Absolute -> [Result] -> [Txt]
    mkRow p xs
        = mkTxt Black (Path.toUnrootedFilePath $ Path.takeFileName p)
        : map mkCell xs

    mkCell :: Result -> Txt
    mkCell ResultOk         = mkTxt Green   "OK"
    mkCell ResultTestFail   = mkTxt Yellow  "TEST"
    mkCell ResultDryFail {} = mkTxt Blue    "NO-IP"
    mkCell ResultDepFail {} = mkTxt Magenta "DEP"
    mkCell ResultFail {}    = mkTxt Red     "FAIL"

    isFail (ResultFail _ o e) = Just (o ++ "\n" ++ e)
    isFail _                  = Nothing

-------------------------------------------------------------------------------
-- matrixRow'
-------------------------------------------------------------------------------

matrixRow' :: Traversable t => Bool -> t GHCVer -> Map PackageName VersionRange -> Path Absolute -> M (Urakka () (t Result))
matrixRow' _test ghcs constraints dir = fmap sequenceA . for ghcs $ \ghcVersion -> do
    uDry <- urakka' $ \() -> do
        (ec, out, err) <- runCabal ModeDry dir ghcVersion constraints
        if isFailure ec
        then return $ Left $ ResultDryFail ec out err
        else return $ Right ()

    uDep <- urakka' $ \() -> do
        (ec, out, err) <- runCabal ModeDep dir ghcVersion constraints
        if isFailure ec
        then return $ Left $ ResultDepFail ec out err
        else return $ Right ()

    uBld <- urakka' $ \() -> do
        (ec, out, err) <- runCabal ModeBuild dir ghcVersion constraints
        if isFailure ec
        then return $ Left $ ResultFail ec out err
        else return $ Right ()

    let u :: Urakka () Result
        u = uDry >>>
            arr Left ||| uDep >>>
            arr Left ||| uBld >>>
            returnA ||| arr (const ResultOk)

    return u
  where
    isFailure (ExitFailure _) = True
    isFailure ExitSuccess     = False
