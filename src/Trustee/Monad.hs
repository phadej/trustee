{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Trustee.Monad (
    M,
    runM,
    putStrs,
    jobs,
    forConcurrently,
    runWithGHC,
    ) where

import Control.Concurrent.STM
       (TVar, atomically, newTVar, readTVar, retry, writeTVar)
import Control.DeepSeq            (force)
import Control.Exception          (bracket, evaluate)
import Control.Monad              (unless)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Foldable              (traverse_)
import System.Exit                (ExitCode (..))
import System.Process             (readProcessWithExitCode)

import qualified Control.Concurrent.Async as Async

import Trustee.Config
import Trustee.GHC
import Trustee.Txt

newtype M a = M { unM :: ReaderT Env IO a }
  deriving newtype (Functor, Applicative, Monad)

data Env = Env
    { envThreads   :: TVar Int
    , envGhcLocks  :: PerGHC (TVar Bool)
    , envPrintLock :: TVar Bool
    , envGhcJobs   :: Int
    , envCabalJobs :: Int
    }

newEnv :: Config -> IO Env
newEnv cfg = atomically $ Env
    <$> newTVar (cfgThreads cfg)
    <*> sequence (pure (newTVar True))
    <*> newTVar True
    <*> pure (cfgGhcJobs cfg)
    <*> pure 1

runM :: Config -> M a -> IO a
runM cfg m = do
    env <- newEnv cfg
    runM' env m

mkM :: (Env -> IO a) -> M a
mkM = M . ReaderT

runM' :: Env -> M a -> IO a
runM' env m = runReaderT (unM m) env

putStrs :: [String] -> M ()
putStrs xs = mkM action where
    action env = bracket acquire release $ \_ -> traverse_ putStrLn xs
      where
        lock = envPrintLock env
        acquire = atomically $ do
            b <- readTVar lock
            unless b retry
            writeTVar lock False
        release _ = atomically $
            writeTVar lock True

forConcurrently :: Traversable t => t a -> (a -> M b) -> M (t b)
forConcurrently xs f = mkM $ \env ->
    Async.forConcurrently xs (runM' env . f)

runWithGHC :: Bool -> GHCVer -> FilePath -> [String] -> M (ExitCode, String, String)
runWithGHC dry ghcVersion cmd args = mkM action
  where
    formatted = cmd ++ " " ++ unwords args
    color ExitSuccess     = Green
    color (ExitFailure _) = if dry then Magenta else Red

    action env = bracket acquire release $ \n -> do
        runM' env $ putStrs [ colored Blue formatted ++ " threads left " ++  show n]
        (ec, o', e') <- readProcessWithExitCode cmd args ""
        runM' env $ putStrs [ colored (color ec) formatted  ]
        o <- evaluate (force o')
        e <- evaluate (force e')
        return (ec, o, e)
      where
        threads
            | dry = 1
            | otherwise = ghcJobs (envGhcJobs env) ghcVersion * envCabalJobs env

        ghcLock = index (envGhcLocks env) ghcVersion
        threadLock = envThreads env

        acquire = atomically $ do
            b <- readTVar ghcLock
            unless b retry

            n <- readTVar threadLock
            unless (n >= threads) retry

            writeTVar ghcLock False
            writeTVar threadLock $! n - threads

            return (n - threads)

        release _ = atomically $ do
            writeTVar ghcLock True
            n <- readTVar threadLock
            writeTVar threadLock $! n + threads

jobs :: M (Int, Int)
jobs = mkM $ \env -> pure (envGhcJobs env, envCabalJobs env)
