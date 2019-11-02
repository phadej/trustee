module Trustee.CabalBuild (
    urakkaCabal,
    urakkaCabalRow,
    CabalResult (..),
    ) where

import Control.Arrow (arr, returnA, (>>>), (|||))

import Trustee.GHC
import Trustee.Monad
import Trustee.Options

import Peura
import Urakka

data CabalResult
    = CabalResultOk
    | CabalResultDryFail !ExitCode !ByteString !ByteString 
    | CabalResultDepFail !ExitCode !ByteString !ByteString 
    | CabalResultFail    !ExitCode !ByteString !ByteString 
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

urakkaCabal
    :: GHCVer                          -- ^ GHC to build with
    -> Map PackageName VersionRange    -- ^ constraints
    -> Path Absolute                   -- ^ working directory
    -> Verify                          -- ^ verify?
    -> M (Urakka () CabalResult)
urakkaCabal ghcVersion constraints dir verify = do
    uDry <- urakka' $ \() -> do
        (ec, out, err) <- runCabal ModeDry dir ghcVersion constraints
        if isFailure ec
        then return $ Left $ CabalResultDryFail ec out err
        else return $ Right ()

    case verify of
        Verify -> do
            uDep <- urakka' $ \() -> do
                (ec, out, err) <- runCabal ModeDep dir ghcVersion constraints
                if isFailure ec
                then return $ Left $ CabalResultDepFail ec out err
                else return $ Right ()

            uBld <- urakka' $ \() -> do
                (ec, out, err) <- runCabal ModeBuild dir ghcVersion constraints
                if isFailure ec
                then return $ Left $ CabalResultFail ec out err
                else return $ Right ()

            let u :: Urakka () CabalResult
                u = uDry >>>
                    arr Left ||| uDep >>>
                    arr Left ||| uBld >>>
                    returnA ||| arr (const CabalResultOk)

            return u

        SolveOnly -> do
            let u :: Urakka () CabalResult
                u = uDry >>> returnA ||| arr (const CabalResultOk)

            return u
  where
    isFailure (ExitFailure _) = True
    isFailure ExitSuccess     = False

urakkaCabalRow :: Traversable t => Verify -> t GHCVer -> Map PackageName VersionRange -> Path Absolute -> M (Urakka () (t CabalResult))
urakkaCabalRow verify ghcs constraints dir = fmap sequenceA . for ghcs $ \ghcVersion ->
    urakkaCabal ghcVersion constraints dir verify
    

