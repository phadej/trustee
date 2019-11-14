{-# LANGUAGE OverloadedStrings #-}
module Trustee.Get where

import Distribution.Version (withinRange)
import System.Directory     (doesDirectoryExist)
import System.FilePath      ((</>))

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified System.Process  as Process

import Trustee.Index
import Trustee.Options

import Peura   hiding ((</>))
import Prelude (putStrLn)

cmdGet :: GlobalOpts -> PackageName -> VersionRange -> Peu r ()
cmdGet opts pkgname vr = liftIO $ do
    index <- readIndex Nothing
    for_ (Map.lookup pkgname index) $ \iv ->
        for_ (indexValueVersions (goIncludeDeprecated opts) iv) $ \v ->
            when (v `withinRange` vr) $ do
                let dirname = prettyShow pkgname ++ "-" ++ prettyShow v
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
