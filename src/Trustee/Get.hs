{-# LANGUAGE OverloadedStrings #-}
module Trustee.Get where

import Distribution.Version (withinRange)
import MiniCurl             (CURL, curlPerform, withCurl)
import Text.Printf          (printf)

import qualified Cabal.Index        as I
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Map.Strict    as Map

import Trustee.Options

import Peura

cmdGet :: TracerPeu r Void -> GlobalOpts -> PackageName -> Maybe VersionRange -> Peu r ()
cmdGet tracer _opts pkgname mvr = withSystemTempDirectory "trustee-get" $ \tmpDir -> do
    cwd <- getCurrentDirectory
    index <- cachedHackageMetadata tracer
    for_ (Map.lookup pkgname index) $ \pkgInfo -> withCurlPeu $ \curl -> do
        let versions0 :: Map Version I.ReleaseInfo
            versions0 = I.piVersions pkgInfo

            versions :: Map Version I.ReleaseInfo
            versions = case mvr of
                Just vr -> Map.filterWithKey (\v _ -> v `withinRange` vr) versions0
                Nothing -> case Map.lookupMax versions0 of
                    Nothing -> Map.empty
                    Just (v, ri) -> Map.singleton v ri

        ifor_ versions $ \v ri -> do
            let dirname    = prettyShow pkgname ++ "-" ++ prettyShow v
            let tarballUrl = "https://hackage.haskell.org/package/" ++ dirname ++ "/" ++ dirname ++ ".tar.gz"
            let cabalUrl   = "https://hackage.haskell.org/package/" ++ dirname ++ "/revision/" ++ show (I.riRevision ri) ++ ".cabal"
            let pkgDir     = cwd </> fromUnrootedFilePath dirname

            exists <- doesDirectoryExist pkgDir
            if exists
            then do
                putInfo tracer $ dirname ++ " exists"
            else do
                bsTarball <- httpGet tracer curl tarballUrl (I.riTarballHash ri) (I.riTarballSize ri)
                bsCabal   <- httpGet tracer curl cabalUrl   (I.riCabalHash ri)   (I.riCabalSize ri)

                -- write tarball to temporary location
                let tmpTarball = tmpDir </> fromUnrootedFilePath (dirname ++ ".tar.gz")
                writeByteString tmpTarball bsTarball

                -- extract the tarball
                _ <- runProcessCheck tracer cwd "tar" ["-xzf", toFilePath tmpTarball ]

                -- write the cabal file
                writeByteString (pkgDir </> fromUnrootedFilePath (prettyShow pkgname ++ ".cabal")) bsCabal

                -- add cabal.project
                writeByteString (pkgDir </> fromUnrootedFilePath "cabal.project") "packages: ."

                -- git
                _ <- runProcessCheck tracer pkgDir "git" ["init"]
                _ <- runProcessCheck tracer pkgDir "git" ["add", "."]
                _ <- runProcessCheck tracer pkgDir "git" ["commit", "-am", "trustee get"]

                return ()

withCurlPeu :: (CURL -> Peu r a) -> Peu r a
withCurlPeu kont = withRunInIO $ \runInIO -> withCurl $ \curl -> runInIO (kont curl)

httpGet :: TracerPeu r Void -> CURL -> String -> I.SHA256 -> Word64 -> Peu r ByteString
httpGet tracer curl url expected' size = do
    putInfo tracer $ printf "Fetching %s (size: %d bytes, sha256: %s)" url size (prettyShow expected')

    bs <- liftIO $ curlPerform curl url (fromIntegral size)
    let actual   = SHA256.hash bs
    let expected = I.getSHA256 expected'

    if actual == expected
    then return bs
    else die tracer "Hash mismatch"
