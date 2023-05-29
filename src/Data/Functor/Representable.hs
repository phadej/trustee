{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE FunctionalDependencies       #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}
{-# LANGUAGE Safe                         #-}
{-# LANGUAGE TypeOperators                #-}
{-# LANGUAGE UndecidableInstances         #-}
-- | Representable functors.
module Data.Functor.Representable (
    -- * Representable functors
    Representable (..),
    -- * Default definitions
    -- ** Applicative
    repPure,
    repAp,
    -- * Generics deriving
    gindex, GIndex,
    gtabulate, GTabulate,
) where

import Data.Functor.Identity (Identity (..))
import Data.Functor.Product  (Product (..))
import GHC.Generics

import Prelude

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

-- | A @'Functor' f@ is 'Representable' if 'tabulate' and 'index' witness an isomorphism to @(->) i@.
--
-- @
-- 'tabulate' . 'index'  ≡ id
-- 'index' . 'tabulate'  ≡ id
-- @
--
-- This is a variant using @FunctionalDependencies@.
--
-- For 'Generic' deriving see 'gindex' and 'gtabulate'.
-- These rely on @'Rep1' f@ and @'Rep' i@ having similar structures.
--
class Representable i f | f -> i where
    index    :: f a -> i -> a
    tabulate :: (i -> a) -> f a

instance Representable () Identity where
    index = gindex
    tabulate = gtabulate

instance (Representable i f, Representable j g) => Representable (Either i j) (Product f g) where
    index (Pair f _) (Left i)  = index f i
    index (Pair _ g) (Right j) = index g j

    tabulate f = Pair (tabulate (f . Left)) (tabulate (f . Right))

instance (Representable i f, Representable j g) => Representable (Either i j) (f :*: g) where
    index (f :*: _) (Left i)  = index f i
    index (_ :*: g) (Right j) = index g j

    tabulate f = tabulate (f . Left) :*: tabulate (f . Right)

-------------------------------------------------------------------------------
-- Combinators
-------------------------------------------------------------------------------

-- | Implementation for 'pure'.
repPure :: Representable i f => a -> f a
repPure = tabulate . const

-- | Implementation for @('<*>')@.
repAp :: Representable i f => f (a -> b) -> f a -> f b
repAp f g = tabulate (index f <*> index g)

-------------------------------------------------------------------------------
-- Generics
-------------------------------------------------------------------------------

-- | Derive 'index' generically.
gindex :: (Generic1 con, Generic idx, GIndex (Rep1 con) (Rep idx)) => con a -> idx -> a
gindex con idx = gindex_ (from1 con) (from idx)
{-# INLINE gindex #-}

-- | A class implementing 'gindex'.
class GIndex con idx where
    gindex_ :: con a -> idx b -> a

instance GIndex con idx => GIndex (M1 i c con) (M1 j k idx) where
    gindex_ (M1 con) (M1 idx) = gindex_ con idx
    {-# INLINE gindex_ #-}

instance GIndex con U1 => GIndex (M1 i c con) U1 where
    gindex_ (M1 con) u1 = gindex_ con u1
    {-# INLINE gindex_ #-}

instance GIndex con (idx :+: idx') => GIndex (M1 i c con) (idx :+: idx') where
    gindex_ (M1 con) idx = gindex_ con idx
    {-# INLINE gindex_ #-}

instance (GIndex con idx, GIndex con' idx') => GIndex (con :*: con') (idx :+: idx') where
    gindex_ (con :*: _)    (L1 idx)  = gindex_ con  idx
    gindex_ (_   :*: con') (R1 idx') = gindex_ con' idx'
    {-# INLINE gindex_ #-}

instance GIndex Par1 U1 where
    gindex_ (Par1 x) _ = x
    {-# INLINE gindex_ #-}

-- | Derive 'tabulate' generically.
gtabulate :: (Generic1 con, Generic idx, GTabulate (Rep1 con) (Rep idx)) => (idx -> a) -> con a
gtabulate f = to1 (gtabulate_ (f . to))
{-# INLINE gtabulate #-}

-- | A class implementing 'gtabulate'.
class GTabulate con idx where
    gtabulate_ :: (idx b -> a) -> con a

instance GTabulate con idx => GTabulate (M1 i c con) (M1 j k idx) where
    gtabulate_ f = M1 (gtabulate_ (f . M1))
    {-# INLINE gtabulate_ #-}

instance GTabulate con U1 => GTabulate (M1 i c con) U1 where
    gtabulate_ f = M1 (gtabulate_ f)
    {-# INLINE gtabulate_ #-}

instance GTabulate con (idx :+: idx') => GTabulate (M1 i c con) (idx :+: idx') where
    gtabulate_ f = M1 (gtabulate_ f)
    {-# INLINE gtabulate_ #-}

instance (GTabulate con idx, GTabulate con' idx') => GTabulate (con :*: con') (idx :+: idx') where
    gtabulate_ f = gtabulate_ (f . L1) :*: gtabulate_ (f . R1)
    {-# INLINE gtabulate_ #-}

instance GTabulate Par1 U1 where
    gtabulate_ f = Par1 (f U1)
    {-# INLINE gtabulate_ #-}
