module Trustee.Get where

import Control.Monad        (when)
import Data.Foldable        (for_)
import Distribution.Package (PackageName)
import Distribution.Text    (display)
import Distribution.Version (VersionRange, withinRange)
import System.Directory     (doesDirectoryExist)

import qualified Data.Map.Strict as Map
import qualified System.Process  as Process

import Trustee.Index

cmdGet :: PackageName -> VersionRange -> IO ()
cmdGet pkgname vr = do
    index <- readIndex
    for_ (Map.lookup pkgname index) $ \vs -> for_ vs $ \v ->
        when (v `withinRange` vr) $ do
            let dirname = display pkgname ++ "-" ++ display v
            exists <- doesDirectoryExist dirname
            if exists
            then putStrLn $ dirname ++ " exists"
            else do
                putStrLn $ "Fetching " ++ dirname
                
                _ <- flip Process.readCreateProcess "" $ Process.proc "cabal" ["get", dirname]
                let indir p = p { Process.cwd = Just dirname }
                _ <- flip Process.readCreateProcess "" $ indir $ Process.proc "git" ["init"]
                _ <- flip Process.readCreateProcess "" $ indir $ Process.proc "git" ["add", "."]
                _ <- flip Process.readCreateProcess "" $ indir $ Process.proc "git" ["commit", "-am", "trustee get"]
  
                pure ()
