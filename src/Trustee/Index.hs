module Trustee.Index (
    Index,
    readIndex,
    indexValueVersions,
  ) where

import Control.Applicative             ((<|>))
import Data.List                       (foldl')
import Data.Time                       (UTCTime)
import Data.Time.Clock.POSIX
       (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Distribution.Compat.CharParsing (char, munch1, string)
import Distribution.Parsec.FieldLineStream (fieldLineStreamFromBS)
import Distribution.Package            (PackageName)
import Distribution.Types.Dependency (Dependency (..))
import Distribution.Parsec.Class       (explicitEitherParsec, parsec, runParsecParser)
import Distribution.Version            (Version, VersionRange, withinRange)
import System.Directory                (getAppUserDataDirectory)
import System.FilePath                 ((</>))

import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Map.Strict         as Map
import qualified Data.Set                as Set

import Trustee.Options (IncludeDeprecated (..))

type Index = Map.Map PackageName IndexValue

data IndexValue = IV
    { _ivVersions  :: !(Set.Set Version)
    , _ivPreferred :: !(Maybe VersionRange)
    }

-- | First arg
indexValueVersions :: IncludeDeprecated -> IndexValue -> Set.Set Version
indexValueVersions IncludeDeprecated (IV vs _)         = vs
indexValueVersions OmitDeprecated    (IV vs Nothing)   = vs
indexValueVersions OmitDeprecated    (IV vs (Just vr)) = Set.filter (`withinRange` vr) vs

singleVersion :: Version -> IndexValue
singleVersion v = IV (Set.singleton v) Nothing

preferred :: VersionRange -> IndexValue
preferred vr = IV Set.empty (Just vr)

-- | Union versions, last preferred
instance Semigroup IndexValue where
    IV v p <> IV v' p' = IV (Set.union v v') (p' <|> p)

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
    add pair@(Pair m _) entry = case explicitEitherParsec p fp of
        Right (Right (pkgName, version)) -> Pair
            (Map.insertWith (<>) pkgName (singleVersion version) m)
            (Tar.entryTime entry)
        Right (Left _) -> case Tar.entryContent entry of
            Tar.NormalFile lbs _ -> case runParsecParser parsec fp (fieldLineStreamFromBS $ LBS.toStrict lbs) of
                Left _    -> pair
                Right (Dependency pkgName vr) -> Pair
                    (Map.insertWith (<>) pkgName (preferred vr) m)
                    (Tar.entryTime entry)
            _ -> pair
        _ -> pair
      where
        fp = Tar.fromTarPathToPosixPath $ Tar.entryTarPath entry

    p = do
        pkgName <- parsec
        _ <- char '/'
        Right <$> versionP pkgName <|> Left <$> preferredP pkgName

    versionP pkgName = do
        version <- parsec
        _ <- char '/'
        _ <- munch1 (const True)
        return (pkgName, version)

    preferredP pkgName = pkgName <$ string "preferred-versions"

