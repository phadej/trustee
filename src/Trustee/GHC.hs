{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
module Trustee.GHC where

import Data.Binary          (Binary)
import Distribution.Version (withinRange)

import GHC.Generics

import Peura

data GHCVer
    = GHC_7_0
    | GHC_7_2
    | GHC_7_4
    | GHC_7_6
    | GHC_7_8
    | GHC_7_10
    | GHC_8_0
    | GHC_8_2
    | GHC_8_4
    | GHC_8_6
    | GHC_8_8
    | GHC_8_10
    | GHC_9_0
    | GHC_9_2
    | GHC_9_4
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance Binary GHCVer

ghcsInRange :: VersionRange -> [GHCVer]
ghcsInRange vr = filter (\v -> withinRange (toVersion v) vr) allGHCVers

allGHCVers :: [GHCVer]
allGHCVers = [minBound .. maxBound]

toVersion :: GHCVer -> Version
toVersion GHC_7_0  = mkVersion [7,0,4]
toVersion GHC_7_2  = mkVersion [7,2,2]
toVersion GHC_7_4  = mkVersion [7,4,2]
toVersion GHC_7_6  = mkVersion [7,6,3]
toVersion GHC_7_8  = mkVersion [7,8,4]
toVersion GHC_7_10 = mkVersion [7,10,3]
toVersion GHC_8_0  = mkVersion [8,0,2]
toVersion GHC_8_2  = mkVersion [8,2,2]
toVersion GHC_8_4  = mkVersion [8,4,4]
toVersion GHC_8_6  = mkVersion [8,6,5]
toVersion GHC_8_8  = mkVersion [8,8,4]
toVersion GHC_8_10 = mkVersion [8,10,7]
toVersion GHC_9_0  = mkVersion [9,0,2]
toVersion GHC_9_2  = mkVersion [9,2,2]
toVersion GHC_9_4  = mkVersion [9,4,0,20220623]

data PerGHC a = PerGHC a a a a a a a a a a a a a a a
  deriving (Functor, Foldable, Traversable, Generic, Generic1)

index :: PerGHC a -> GHCVer -> a
index = gindex

tabulate :: (GHCVer -> a) -> PerGHC a
tabulate = gtabulate

instance Applicative PerGHC where
    pure = tabulate . const
    f <*> x = tabulate (\i -> index f i (index x i))

ghcJobs :: Int -> GHCVer -> Int
ghcJobs n v
    | v < GHC_7_8 = 1
    | otherwise   = n

ghcOptions :: Int -> GHCVer -> String
ghcOptions n v
    | v < GHC_7_8 = ghcRtsOpts
    | otherwise   = "-j" ++ show n ++ " " ++ ghcRtsOpts

ghcRtsOpts :: String
ghcRtsOpts = "+RTS -A64m -I0 -qg -RTS"

-------------------------------------------------------------------------------
-- gindex and gtabulate
-------------------------------------------------------------------------------

gindex :: (Generic1 con, Generic idx, GIndex (Rep1 con) (Rep idx)) => con a -> idx -> a
gindex con idx = gindex_ (from1 con) (from idx)
{-# INLINE gindex #-}

class GIndex con idx where
    gindex_ :: con a -> idx b -> a

instance GIndex con idx => GIndex (M1 i c con) (M1 j k idx) where
    gindex_ (M1 con) (M1 idx) = gindex_ con idx
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

gtabulate :: (Generic1 con, Generic idx, GTabulate (Rep1 con) (Rep idx)) => (idx -> a) -> con a
gtabulate f = to1 (gtabulate_ (f . to))
{-# INLINE gtabulate #-}

class GTabulate con idx where
    gtabulate_ :: (idx b -> a) -> con a

instance GTabulate con idx => GTabulate (M1 i c con) (M1 j k idx) where
    gtabulate_ f = M1 (gtabulate_ (f . M1))
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
