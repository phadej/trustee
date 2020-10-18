module Trustee.Index (
    Index,
    readIndex,
    indexValueVersions,
  ) where

import Data.Time                           (UTCTime)
import Data.Time.Clock.POSIX
       (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Distribution.Compat.CharParsing     (char, munch1, string)
import Distribution.Package                (PackageName)
import Distribution.Parsec
       (explicitEitherParsec, parsec, runParsecParser)
import Distribution.Parsec.FieldLineStream (fieldLineStreamFromBS)
import Distribution.Types.Dependency       (Dependency (..))
import Distribution.Version                (withinRange)
import System.Directory                    (getAppUserDataDirectory)
import System.FilePath                     ((</>))

import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Map.Strict         as Map
import qualified Data.Set                as Set

import Trustee.Options (IncludeDeprecated (..))

import Peura hiding ((</>), getAppUserDataDirectory)
import Cabal.Index
import Cabal.Config

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

readIndex :: Maybe UTCTime -> IO Index
readIndex Nothing = do
    (_, meta) <- Cabal.Index.cachedHackageMetadata
    return $ fmap fromPi meta
readIndex (Just indexState) = do
    cfg <- readConfig
    indexPath <- maybe
        (fail "No hackage.haskell.org repo")
        return
        (cfgRepoIndex cfg hackageHaskellOrg)
    meta <- indexMetadata indexPath (Just (truncate (utcTimeToPOSIXSeconds indexState)))
    return $ fmap fromPi meta

fromPi p = IV (Map.keysSet (piVersions p)) (Just (piPreferred p))
