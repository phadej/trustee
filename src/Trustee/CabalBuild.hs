module Trustee.CabalBuild (
    urakkaCabal,
    urakkaCabalRow,
    CabalResult (..),
    ) where

import Control.Arrow          (arr, (>>>), (|||))
import Control.Concurrent.STM (STM)

import Trustee.GHC
import Trustee.Monad
import Trustee.Options

import Peura
import Urakka

data CabalResult
    = CabalResultPending
    | CabalResultOk
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
    -> M (STM CabalResult, Urakka () CabalResult)
urakkaCabal ghcV constraints dir verify = do
    uDry <- urakka' $ \() -> do
        (ec, out, err) <- runCabal ModeDry dir ghcV constraints
        if isFailure ec
        then return $ Left $ CabalResultDryFail ec out err
        else return $ Right ()

    (stm, post) <- urakkaSTM $ return . either id (const CabalResultOk)

    case verify of
        Verify -> do
            uDep <- urakka' $ \() -> do
                (ec, out, err) <- runCabal ModeDep dir ghcV constraints
                if isFailure ec
                then return $ Left $ CabalResultDepFail ec out err
                else return $ Right ()

            uBld <- urakka' $ \() -> do
                (ec, out, err) <- runCabal ModeBuild dir ghcV constraints
                if isFailure ec
                then return $ Left $ CabalResultFail ec out err
                else return $ Right ()

            let u :: Urakka () CabalResult
                u = uDry >>>
                    arr Left ||| uDep >>>
                    arr Left ||| uBld >>>
                    post

            return (stm, u)

        SolveOnly -> do

            let u :: Urakka () CabalResult
                u = uDry >>> post

            return (stm, u)
  where
    isFailure (ExitFailure _) = True
    isFailure ExitSuccess     = False

urakkaCabalRow
    :: Traversable t => Verify -> t GHCVer -> Map PackageName VersionRange -> Path Absolute
    -> M (t (STM CabalResult), Urakka () (t CabalResult))
urakkaCabalRow verify ghcs constraints dir
    = fmap (\xs -> (fmap fst xs, traverse snd xs))
    . for ghcs $ \ghcV ->
        urakkaCabal ghcV constraints dir verify


