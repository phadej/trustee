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
import Path                       (Abs, Dir, Path)
import System.Exit                (ExitCode (..))

import qualified Control.Concurrent.Async as Async
import qualified Path
import qualified System.Process           as Process

import Trustee.Config
import Trustee.GHC
import Trustee.Txt

newtype M a = M { unM :: ReaderT Env IO a }
  deriving newtype (Functor, Applicative, Monad)

data Env = Env
    { envThreads   :: TVar Int
    , envInQueue   :: TVar Int
    , envGhcLocks  :: PerGHC (TVar Bool)
    , envPrintLock :: TVar Bool
    , envGhcJobs   :: Int
    , envCabalJobs :: Int
    }

newEnv :: Config -> IO Env
newEnv cfg = atomically $ Env
    <$> newTVar (cfgThreads cfg)
    <*> newTVar 0
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

runWithGHC :: Bool -> Path Abs Dir -> GHCVer -> FilePath -> [String] -> M (ExitCode, String, String)
runWithGHC dry dir ghcVersion cmd args = mkM action
  where
    dir' = Path.toFilePath $ Path.dirname dir
    formatted = cmd ++ " " ++ unwords args
    color ExitSuccess     = Green
    color (ExitFailure _) = if dry then Magenta else Red

    action env = bracket acquire release $ \(m, n) -> do
        runM' env $ putStrs [ dir' ++ " " ++ colored Blue formatted ++ " " ++ show n ++ "/" ++ show m]
        let process = (Process.proc cmd args) { Process.cwd = Just $ Path.toFilePath dir }
        (ec, o', e') <- Process.readCreateProcessWithExitCode process ""
        runM' env $ putStrs [ dir' ++ " " ++ colored (color ec) formatted  ]
        o <- evaluate (force o')
        e <- evaluate (force e')
        return (ec, o, e)
      where
        threads
            | dry = 1
            | otherwise = ghcJobs (envGhcJobs env) ghcVersion * envCabalJobs env

        ghcLock = index (envGhcLocks env) ghcVersion
        threadLock = envThreads env
        inQueueTVar = envInQueue env

        acquire = do
            atomically $ do
                m <- readTVar inQueueTVar
                let m' = m + 1
                writeTVar inQueueTVar $! m'

            atomically $ do
                b <- readTVar ghcLock
                unless b retry

                n <- readTVar threadLock
                unless (n >= threads) retry

                writeTVar ghcLock False
                let n' = n - threads
                writeTVar threadLock $! n'

                m <- readTVar inQueueTVar

                return (m, n')

        release _ = atomically $ do
            writeTVar ghcLock True
            n <- readTVar threadLock
            writeTVar threadLock $! n + threads
            m <- readTVar inQueueTVar
            writeTVar inQueueTVar $! m - 1

jobs :: M (Int, Int)
jobs = mkM $ \env -> pure (envGhcJobs env, envCabalJobs env)
