{-# LANGUAGE OverloadedStrings #-}
module Trustee.Get where

import Control.Monad        (when)
import Data.Foldable        (for_)
import Distribution.Package (PackageName)
import Distribution.Text    (display)
import Distribution.Version (VersionRange, withinRange)
import System.Directory     (doesDirectoryExist)
import System.FilePath      ((</>))

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified System.Process  as Process

import Trustee.Index
import Trustee.Options

cmdGet :: GlobalOpts -> PackageName -> VersionRange -> IO ()
cmdGet opts pkgname vr = do
    index <- fst <$> readIndex Nothing
    for_ (Map.lookup pkgname index) $ \iv ->
        for_ (indexValueVersions (goIncludeDeprecated opts) iv) $ \v ->
            when (v `withinRange` vr) $ do
                let dirname = display pkgname ++ "-" ++ display v
                exists <- doesDirectoryExist dirname
                if exists
                then putStrLn $ dirname ++ " exists"
                else do
                    putStrLn $ "Fetching " ++ dirname

                    _ <- flip Process.readCreateProcess "" $ Process.proc "cabal" ["get", dirname]
                    let indir p = p { Process.cwd = Just dirname }

                    -- add cabal.project
                    BS.writeFile (dirname </> "cabal.project") "packages: ."

                    -- git
                    _ <- flip Process.readCreateProcess "" $ indir $ Process.proc "git" ["init"]
                    _ <- flip Process.readCreateProcess "" $ indir $ Process.proc "git" ["add", "."]
                    _ <- flip Process.readCreateProcess "" $ indir $ Process.proc "git" ["commit", "-am", "trustee get"]

                    pure ()
