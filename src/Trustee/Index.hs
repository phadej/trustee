module Trustee.Index (Index, readIndex) where

import Data.List                 (foldl')
import Data.Time                 (UTCTime)
import Data.Time.Clock.POSIX     (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
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

data Pair = Pair !Index !Tar.EpochTime

toLazy :: Pair -> (Index, UTCTime)
toLazy (Pair index ts) = (index, posixSecondsToUTCTime $ fromIntegral ts)

readIndex :: Maybe UTCTime -> IO (Index, UTCTime)
readIndex indexState = do
    c <- getAppUserDataDirectory "cabal"
    -- TODO: we could read ~/.cabal/config
    let indexTar = c </> "packages/hackage.haskell.org/01-index.tar"
    contents <- LBS.readFile indexTar
    return $ toLazy $ foldl' add start $ entriesToList $ Tar.read contents
  where
    start = Pair mempty 0

    predicate = case indexState of
        Just t  -> \entry -> Tar.entryTime entry <= truncate (utcTimeToPOSIXSeconds t)
        Nothing -> const True

    entriesToList :: Tar.Entries Tar.FormatError -> [Tar.Entry]
    entriesToList Tar.Done        = []
    entriesToList (Tar.Fail s)    = error (show s)
    entriesToList (Tar.Next e es)
        | predicate e = e : entriesToList es
        | otherwise   = entriesToList es

    add :: Pair -> Tar.Entry -> Pair
    add pair@(Pair m _) entry = case maybeReadP p $ Tar.fromTarPathToPosixPath $ Tar.entryTarPath entry of
        Just (pkgName, version) -> Pair
            (Map.insertWith Set.union pkgName (Set.singleton version) m)
            (Tar.entryTime entry)
        _ -> pair

    p = do
        pkgName <- parse
        _ <- char '/'
        version <- parse
        _ <- char '/'
        _ <- munch1 (const True)
        return (pkgName, version)

