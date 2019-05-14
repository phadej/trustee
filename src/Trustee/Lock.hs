{-# LANGUAGE BangPatterns #-}
module Trustee.Lock (withLock) where

import Control.Concurrent        (threadDelay)
import Control.Exception         (bracket, handle)
import Control.Monad             (when)
import Data.Foldable             (for_)
import Data.Function             ((&))
import GHC.IO.Handle.Lock        (LockMode (ExclusiveLock), hTryLock)
import System.Console.Concurrent (outputConcurrent)
import System.Console.Regions
       (RegionLayout (Linear), closeConsoleRegion, setConsoleRegion, displayConsoleRegions,
       withConsoleRegion)
import System.Directory
       (XdgDirectory (..), createDirectoryIfMissing, emptyPermissions,
       getXdgDirectory, setOwnerExecutable, setOwnerReadable,
       setOwnerSearchable, setOwnerWritable, setPermissions)
import System.FilePath           ((</>))
import System.IO
       (IOMode (ReadWriteMode), hClose, hFlush, openFile)
import System.Posix.Process      (getProcessID)
import System.Posix.Types        (CPid (..))
import System.Process            (readProcessWithExitCode)
import Text.Read                 (readMaybe)

withLock :: IO a -> IO a
withLock action = do
    cacheDir <- getXdgDirectory XdgCache "trustee"
    createDirectoryIfMissing True cacheDir
    setPermissions cacheDir $ emptyPermissions
        & setOwnerReadable True
        & setOwnerWritable True
        & setOwnerExecutable True
        & setOwnerSearchable True

    let lockFile = cacheDir </> "lock"
    let pidFile  = cacheDir </> "pid"
    let open = openFile lockFile ReadWriteMode
    displayConsoleRegions $ withConsoleRegion Linear $ \region ->
        bracket open hClose (loop (0 :: Integer) region lockFile pidFile)
  where
    doNothing :: IOError -> IO ()
    doNothing _ = return ()

    divides :: Integral a => a -> a -> Bool
    divides d x = x `mod` d == 0

    loop !n region lockFile pidFile h = do
        l <- hTryLock h ExclusiveLock
        if l
        then do
            CPid pid <- getProcessID
            writeFile pidFile (show pid)
            hFlush h
            closeConsoleRegion region
            action
        else do
            when (n < 10 || n < 300 && 10 `divides` n || 300 `divides` n) $ outputConcurrent $
                if n < 300
                then "other trustee process running, waited " ++ show n ++ " secs\n"
                else "other trustee process running, waited " ++ show (n `div` 60) ++ " mins\n"

            handle doNothing $ do
                mpid <- readMaybe <$> readFile pidFile
                for_ mpid $ \pid -> do
                    (_, out, _) <- readProcessWithExitCode "ps" ["-f", "--cumulative", show (pid :: Int)] ""
                    setConsoleRegion region $ "FOO " ++ out

            -- wait 5 seconds, and try again
            threadDelay $ 1 * 1000 * 1000
            loop (succ n) region lockFile pidFile h
