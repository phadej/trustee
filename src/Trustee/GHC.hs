{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
module Trustee.GHC where

import Data.Binary                (Binary)
import Data.Functor.Representable (Representable (..), gindex, gtabulate, repAp, repPure)
import Distribution.Version       (withinRange)

import Peura

data GHCVer
    = GHC_8_4
    | GHC_8_6
    | GHC_8_8
    | GHC_8_10
    | GHC_9_0
    | GHC_9_2
    | GHC_9_4
    | GHC_9_6
    | GHC_9_8
    | GHC_9_10
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance Binary GHCVer

ghcsInRange :: VersionRange -> [GHCVer]
ghcsInRange vr = filter (\v -> withinRange (toVersion v) vr) allGHCVers

allGHCVers :: [GHCVer]
allGHCVers = [minBound .. maxBound]

toVersion :: GHCVer -> Version
toVersion GHC_8_4  = mkVersion [8,4,4]
toVersion GHC_8_6  = mkVersion [8,6,5]
toVersion GHC_8_8  = mkVersion [8,8,4]
toVersion GHC_8_10 = mkVersion [8,10,7]
toVersion GHC_9_0  = mkVersion [9,0,2]
toVersion GHC_9_2  = mkVersion [9,2,8]
toVersion GHC_9_4  = mkVersion [9,4,8]
toVersion GHC_9_6  = mkVersion [9,6,5]
toVersion GHC_9_8  = mkVersion [9,8,2]
toVersion GHC_9_10 = mkVersion [9,10,1]

data PerGHC a = PerGHC a a a a a a a a a a
  deriving (Functor, Foldable, Traversable, Generic, Generic1)

instance Representable GHCVer PerGHC where
    index = gindex
    tabulate = gtabulate

instance Applicative PerGHC where
    pure = repPure
    (<*>) = repAp

ghcJobs :: Int -> GHCVer -> Int
ghcJobs n v
    | otherwise   = n

ghcOptions :: Int -> GHCVer -> String
ghcOptions n v
    | otherwise   = "-j" ++ show n ++ " " ++ ghcRtsOpts

ghcRtsOpts :: String
ghcRtsOpts = "+RTS -A64m -I0 -qg -RTS"
