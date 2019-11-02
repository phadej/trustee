-- |
-- SDPX-License-Identifier: GPL-2.0-or-later
-- Copyright: Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Collection of exports from the dependencies.
--
module Peura.Exports (
    module A,
    -- * Classes
    Alternative (..),
    Coercible,
    Generic,
    Generic1,
    MonadIO (..),
    MonadUnliftIO (..),
    MonadReader (..),
    MonadThrow (..),
    MonadCatch (..),
    MonadMask (..),
    NFData (..),
    IsString (..),
    Serialise,
    -- * Types
    ByteString,
    ExitCode (..),
    LazyByteString,
    Map,
    NonEmpty (..),
    PackageName,
    PackageId,
    Set,
    Text,
    Version,
    VersionRange,
    -- * Individual functions
    -- * Control.DeepSeq
    force,
    -- ** Control.Monad
    ap,
    foldM,
    unless,
    when,
    (<$!>),
    -- ** Control.Monad.Catch
    handle,
    bracket,
    -- ** Data.Coercible
    coerce,
    -- ** Data.Foldable
    for_,
    traverse_,
    -- ** Data.Maybe
    fromMaybe,
    mapMaybe,
    -- ** Data.List
    sortBy, sortOn,
    -- ** Data.List.NonEmpty
    head, last, groupBy,
    -- ** Data.Traversable
    for,
    -- * Cabal
    prettyShow,
    mkPackageName,
    mkVersion,
    -- ** UTF8
    fromUTF8BS, toUTF8BS,
    -- * Helpers from lens
    ifor,
    ifor_,
    ix,
    at,
    -- * Lens operators
    (^?),
    -- * Developing
    error,
    undefined,
    ) where

-- to get all members of Foldable
import Data.Foldable as A (Foldable (..))
import Prelude       as A
       (Applicative (..), Bool (..), Bounded (..), Double, Either (..),
       Enum (..), Eq (..), FilePath, Fractional (..), Functor (..), IO, Int,
       Integer, Integral (..), Maybe (..), Monad (..), Monoid (..), Num (..),
       Ord (..), Rational, Real (..), RealFrac (..), Semigroup (..), Show (..),
       String, Traversable (..), all, and, any, concat, concatMap, const,
       curry, dropWhile, either, filter, flip, fromIntegral, fst, id, length,
       map, maybe, not, or, otherwise, realToFrac, replicate, return, reverse,
       snd, span, take, takeWhile, uncurry, unlines, unwords, zipWith, ($),
       ($!), (&&), (++), (.), (<$>), (||))

import Codec.Serialise                 (Serialise)
import Control.Applicative             (Alternative (..))
import Control.DeepSeq                 (NFData (..), force)
import Control.Lens                    (at, ifor, ifor_, ix, (^?))
import Control.Monad                   (ap, foldM, unless, when, (<$!>))
import Control.Monad.Catch
       (MonadCatch (..), MonadMask (..), MonadThrow (..), bracket, handle)
import Control.Monad.IO.Class          (MonadIO (..))
import Control.Monad.IO.Unlift         (MonadUnliftIO (withRunInIO))
import Control.Monad.Reader.Class      (MonadReader (ask, local))
import Data.ByteString                 (ByteString)
import Data.Coerce                     (Coercible, coerce)
import Data.Foldable                   (for_, traverse_)
import Data.List                       (sortBy, sortOn)
import Data.List.NonEmpty              (NonEmpty (..), groupBy, head, last)
import Data.Map.Strict                 (Map)
import Data.Maybe                      (fromMaybe, mapMaybe)
import Data.Set                        (Set)
import Data.String                     (IsString (..))
import Data.Text                       (Text)
import Data.Traversable                (for)
import Distribution.Pretty             (prettyShow)
import Distribution.Simple.Utils       (fromUTF8BS, toUTF8BS)
import Distribution.Types.PackageId    (PackageId)
import Distribution.Types.PackageName  (PackageName, mkPackageName)
import Distribution.Types.Version      (Version, mkVersion)
import Distribution.Types.VersionRange (VersionRange)
import GHC.Generics                    (Generic, Generic1)
import System.Exit                     (ExitCode (..))

-- We use generic-lens with OverloadedLabels
import Data.Generics.Labels ()

import qualified Data.ByteString.Lazy as LBS

import qualified Prelude

type LazyByteString = LBS.ByteString

undefined :: a
undefined = Prelude.undefined
{-# DEPRECATED undefined "Don't leave me in the code" #-}

error :: String -> a
error = Prelude.error
{-# DEPRECATED error "Don't leave me in the code" #-}
