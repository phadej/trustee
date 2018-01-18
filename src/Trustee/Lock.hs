module Trustee.Lock (withLock) where

import Control.Concurrent        (threadDelay)
import Control.Exception         (bracket, handle)
import Data.Foldable             (for_)
import Data.Function             ((&))
import GHC.IO.Handle.Lock        (LockMode (ExclusiveLock), hTryLock)
import System.Console.Concurrent (outputConcurrent)
import System.Console.Regions
       (RegionLayout (Linear), closeConsoleRegion, setConsoleRegion,
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
    withConsoleRegion Linear $ \region ->
        bracket open hClose (loop region lockFile pidFile)
  where
    doNothing :: IOError -> IO ()
    doNothing _ = return ()

    loop region lockFile pidFile h = do
        l <- hTryLock h ExclusiveLock
        if l
        then do
            CPid pid <- getProcessID
            writeFile pidFile (show pid)
            hFlush h
            closeConsoleRegion region
            action
        else do
            outputConcurrent "other trustee process running"
            handle doNothing $ do
                mpid <- readMaybe <$> readFile pidFile
                for_ mpid $ \pid -> do
                    (_, out, _) <- readProcessWithExitCode "ps" ["-f", "--cumulative", show (pid :: Int)] ""
                    setConsoleRegion region out

            -- wait 5 seconds, and try again
            threadDelay $ 5 * 1000 * 1000
            loop region lockFile pidFile h
