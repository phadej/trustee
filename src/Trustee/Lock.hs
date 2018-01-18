module Trustee.Lock (withLock) where

import Control.Concurrent   (threadDelay)
import Control.Exception    (bracket, handle)
import Data.Foldable        (for_)
import Data.Function        ((&))
import GHC.IO.Handle.Lock   (LockMode (ExclusiveLock), hTryLock)
import System.Directory
       (XdgDirectory (..), createDirectoryIfMissing, emptyPermissions,
       getXdgDirectory, setOwnerExecutable, setOwnerReadable,
       setOwnerSearchable, setOwnerWritable, setPermissions)
import System.FilePath      ((</>))
import System.IO
       (IOMode (ReadWriteMode), hClose, hFlush, hPutStrLn, openFile, stderr)
import System.Posix.Process (getProcessID)
import System.Posix.Types   (CPid (..))
import System.Process       (callProcess)
import Text.Read            (readMaybe)

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
    let close h = hClose h
    bracket open close (loop lockFile pidFile 16)
  where
    doNothing :: IOError -> IO ()
    doNothing _ = return ()

    loop lockFile pidFile interval h = do
        l <- hTryLock h ExclusiveLock
        if l
        then do
            CPid pid <- getProcessID
            writeFile pidFile (show pid)
            hFlush h
            action
        else do
            hPutStrLn stderr $ "other trustee process running"
            handle doNothing $ do
                mpid <- readMaybe <$> readFile pidFile
                for_ mpid $ \pid -> callProcess "ps" ["-f", "--cumulative", show (pid :: Int)]
            hPutStrLn stderr $ "sleeping for " ++ show interval ++ " seconds"

            -- wait 10 seconds, and try again
            threadDelay $ interval * 1000 * 1000
            loop lockFile pidFile (min 300 $ interval + interval `div` 2) h
