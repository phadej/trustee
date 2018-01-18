{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
module Trustee.GHC where

import Data.Binary          (Binary)
import Distribution.Version (Version, VersionRange, mkVersion, withinRange)
import GHC.Generics         (Generic)

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
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

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
toVersion GHC_8_4  = mkVersion [8,4,3]
toVersion GHC_8_6  = mkVersion [8,6,1]

data PerGHC a = PerGHC a a a a a a a a a a
  deriving (Functor, Foldable, Traversable)

index :: PerGHC a -> GHCVer -> a
index (PerGHC x _ _ _ _ _ _ _ _ _) GHC_7_0 = x
index (PerGHC _ x _ _ _ _ _ _ _ _) GHC_7_2 = x
index (PerGHC _ _ x _ _ _ _ _ _ _) GHC_7_4 = x
index (PerGHC _ _ _ x _ _ _ _ _ _) GHC_7_6 = x
index (PerGHC _ _ _ _ x _ _ _ _ _) GHC_7_8 = x
index (PerGHC _ _ _ _ _ x _ _ _ _) GHC_7_10 = x
index (PerGHC _ _ _ _ _ _ x _ _ _) GHC_8_0 = x
index (PerGHC _ _ _ _ _ _ _ x _ _) GHC_8_2 = x
index (PerGHC _ _ _ _ _ _ _ _ x _) GHC_8_4 = x
index (PerGHC _ _ _ _ _ _ _ _ _ x) GHC_8_6 = x

tabulate :: (GHCVer -> a) -> PerGHC a
tabulate f = PerGHC
    (f GHC_7_0)
    (f GHC_7_2)
    (f GHC_7_4)
    (f GHC_7_6)
    (f GHC_7_8)
    (f GHC_7_10)
    (f GHC_8_0)
    (f GHC_8_2)
    (f GHC_8_4)
    (f GHC_8_6)

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
