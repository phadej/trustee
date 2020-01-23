{-# LANGUAGE BangPatterns #-}
module Trustee.Lock (withLock) where

import Control.Concurrent     (threadDelay)
import Control.Exception      (IOException)
import Data.Function          ((&))
import Lukko
       (LockMode (ExclusiveLock), fdClose, fdOpen, fdTryLock, fdUnlock)
import System.Console.Regions
       (RegionLayout (Linear), closeConsoleRegion, displayConsoleRegions,
       setConsoleRegion, withConsoleRegion)
import System.Directory
       (XdgDirectory (..), createDirectoryIfMissing, emptyPermissions,
       getXdgDirectory, setOwnerExecutable, setOwnerReadable,
       setOwnerSearchable, setOwnerWritable, setPermissions)
import System.FilePath        ((</>))
import System.Posix.Process   (getProcessID)
import System.Posix.Types     (CPid (..))
import System.Process         (readProcessWithExitCode)
import Text.Read              (readMaybe)

import Peura hiding (createDirectoryIfMissing, (</>))
import Prelude (writeFile, readFile)

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
    let close fd = fdUnlock fd *> fdClose fd
    displayConsoleRegions $ withConsoleRegion Linear $ \region ->
        bracket (fdOpen lockFile) close $
            loop (0 :: Integer) region lockFile pidFile
  where
    doNothing :: IOException -> IO String
    doNothing _ = return ""

    divides :: Integral a => a -> a -> Bool
    divides d x = x `mod` d == 0

    loop !n region lockFile pidFile fd = do
        l <- fdTryLock fd ExclusiveLock
        if l
        then do
            CPid pid <- getProcessID
            writeFile pidFile (show pid)
            closeConsoleRegion region
            action
        else do
            when (n < 10 || 10 `divides` n) $ do
                let fstLine | n < 300   = "other trustee process running, waited " ++ show n ++ "s\n"
                            | otherwise = "other trustee process running, waited " ++ show (n `div` 60) ++ "m" ++ show (n `mod` 60) ++ "s\n"

                sndLine <- handle doNothing $ do
                    mpid <- readMaybe <$> readFile pidFile
                    case mpid of
                        Nothing  -> return ""
                        Just pid -> do
                            (_, out, _) <- readProcessWithExitCode "ps" ["-f", "--cumulative", show (pid :: Int)] ""
                            return out

                setConsoleRegion region $ fstLine ++ sndLine

            -- wait a second, and try again
            threadDelay $ 1 * 1000 * 1000
            loop (succ n) region lockFile pidFile fd
