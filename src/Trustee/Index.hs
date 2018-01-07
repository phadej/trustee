module Trustee.Index (readIndex) where

import Data.List                 (foldl')
import Distribution.Compat.ReadP (char, munch1)
import Distribution.Package      (PackageName)
import Distribution.Text         (parse)
import Distribution.Version      (Version)
import System.Directory          (getAppUserDataDirectory)
import System.FilePath           ((</>))

import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Map.Strict         as Map
import qualified Data.Set                as Set

import Trustee.Util

type Index = Map.Map PackageName (Set.Set Version)

readIndex :: IO Index
readIndex = do
    c <- getAppUserDataDirectory "cabal"
    -- TODO: we could read ~/.cabal/config
    let indexTar = c </> "packages/hackage.haskell.org/01-index.tar"
    contents <- LBS.readFile indexTar
    return $ foldl' add Map.empty $ entriesToList $ Tar.read contents
  where
    entriesToList :: Tar.Entries Tar.FormatError -> [Tar.Entry]
    entriesToList Tar.Done        = []
    entriesToList (Tar.Fail s)    = error (show s)
    entriesToList (Tar.Next e es) = e : entriesToList es

    add m entry = case maybeReadP p $ Tar.fromTarPathToPosixPath $ Tar.entryTarPath entry of
        Just (pkgName, version) ->
            Map.insertWith Set.union pkgName (Set.singleton version) m
        _ -> m

    p = do
        pkgName <- parse
        _ <- char '/'
        version <- parse
        _ <- char '/'
        _ <- munch1 (const True)
        return (pkgName, version)

