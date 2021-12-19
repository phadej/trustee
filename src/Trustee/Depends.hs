module Trustee.Depends (
    allBuildDepends,
    extractMajorVersion,
) where

import Peura

import Algebra.Lattice                 (BoundedMeetSemiLattice (top), Lattice (..), meets)
import Distribution.Compiler           (CompilerFlavor (..))
import Distribution.PackageDescription (GenericPackageDescription (..))
import Distribution.System             (OS (..))
import Distribution.Types.CondTree     (CondBranch (..), CondTree (..), mapTreeConstrs)
import Distribution.Types.Condition    (simplifyCondition)
import Distribution.Types.Dependency   (Dependency (..))
import Distribution.Version
       (intersectVersionRanges, simplifyVersionRange, unionVersionRanges, versionNumbers, withinRange)

import qualified Data.Map.Strict                 as Map
import qualified Distribution.PackageDescription as PD

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

allBuildDepends :: Version -> GenericPackageDescription -> Map.Map PackageName VersionRange
allBuildDepends ghcVer
    = fmap simplifyVersionRange
    . extractBuildDepends
  where
    extractBuildDepends :: GenericPackageDescription -> Map.Map PackageName VersionRange
    extractBuildDepends gpd =
        maybe Map.empty libBD (condLibrary gpd)

    libBD :: PD.CondTree PD.ConfVar [Dependency] PD.Library
          -> Map.Map PackageName VersionRange
    libBD
        = unDepMap
        . simplifyCondTree' evalConfVar
        . mapTreeConstrs toDepMap


    evalConfVar :: PD.ConfVar -> Either PD.ConfVar Bool
    evalConfVar (PD.OS Linux)    = Right True
    evalConfVar (PD.OS _)        = Right False
    evalConfVar (PD.Impl GHC vr) = Right $ ghcVer `withinRange` vr
    evalConfVar (PD.Impl _ _)    = Right False
    evalConfVar c                = Left c

newtype DepMap = DepMap { unDepMap :: Map.Map PackageName VersionRange }

instance Lattice DepMap where
    -- here we want to union dependency ranges
    DepMap x \/ DepMap y = DepMap (Map.unionWith unionVersionRanges x y)
    DepMap x /\ DepMap y = DepMap (Map.unionWith unionVersionRanges x y)

instance BoundedMeetSemiLattice DepMap where
    top = DepMap Map.empty

toDepMap :: [Dependency] -> DepMap
toDepMap =
    DepMap . Map.fromListWith intersectVersionRanges . map dependencyToPair
  where
    dependencyToPair (Dependency p vr _) = (p, vr)

-- | Like 'simplifyCondTree', but when a condition cannot be evaluated, both branches are included.
simplifyCondTree'
    :: (Lattice d, BoundedMeetSemiLattice d)
    => (v -> Either v Bool)
    -> PD.CondTree v d a
    -> d
simplifyCondTree' env (CondNode _a d ifs) =
    d /\ meets (map simplifyIf ifs)
  where
    simplifyIf (CondBranch cnd t me) =
        case simplifyCondition cnd env of
              (PD.Lit True, _)  -> t'
              (PD.Lit False, _) -> e'
              (_, _)            -> t' \/  e'
      where
        t' = simplifyCondTree' env t
        e' = maybe top (simplifyCondTree' env) me

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

extractMajorVersion :: Version -> Version
extractMajorVersion v = case versionNumbers v of
    []          -> mkVersion [0]
    [_]         -> v
    [_,_]       -> v
    (x : y : _) -> mkVersion [x, y]
