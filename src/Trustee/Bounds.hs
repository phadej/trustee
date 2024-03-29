{-# LANGUAGE DeriveGeneric #-}
module Trustee.Bounds (cmdBounds) where

import Control.Arrow                          (returnA, (+++), (>>>), (|||))
import Control.Concurrent.STM                 (STM, orElse)
import Data.Function                          (on)
import Data.Maybe                             (listToMaybe)
import Data.Semigroup                         (Max (..), Min (..))
import Data.Semigroup.Foldable                (Foldable1 (..))
import Distribution.Package                   (PackageName)
import Distribution.PackageDescription        (GenericPackageDescription (..))
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Version
       (intersectVersionRanges, orEarlierVersion, orLaterVersion, thisVersion, versionNumbers, withinRange)
import Prelude                                (userError)
import System.Path                            (Absolute, Path)

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict    as Map
import qualified Data.Set           as Set

import Trustee.Depends
import Trustee.GHC     hiding (index)
import Trustee.Index
import Trustee.Monad
import Trustee.Options
import Trustee.Table
import Trustee.Txt

import Peura
import Urakka

-- We'd like to have Foldable1?
import Prelude (maximum, minimum)

cmdBounds :: TracerPeu Env Void -> GlobalOpts -> Path Absolute -> Verify -> Limit -> M (STM String, Urakka () ())
cmdBounds tracer opts dir verify limit = do
    let ghcs = ghcsInRange (goGhcVersions opts)
    xs <- globDir1 "*.cabal" dir

    case xs of
        [cabalFile] -> do
            putInfo tracer "Reading hackage index"
            index' <- liftIO $ readIndex (goIndexState opts)
            let index = indexValueVersions (goIncludeDeprecated opts) <$> index'

            putInfo tracer "Reading cabal file"
            gpd <- liftIO $ readGenericPackageDescription maxBound (toFilePath cabalFile)

            colsR <- for ghcs $ \ghcVer -> do
                cells <- boundsForGhc verify limit dir index gpd ghcVer
                let addVersion :: Map PackageName a -> Map (GHCVer, PackageName) a
                    addVersion = Map.mapKeys ((,) ghcVer)
                return $ bimap addVersion (fmap addVersion) cells

            let stms :: STM (Map (GHCVer, PackageName) Result)
                stms = traverse (`orElse` return ResultPending)
                     $ Map.unions $ map fst colsR

                cols :: Urakka () (Map (GHCVer, PackageName) Result)
                cols = Map.unions <$> traverse snd colsR

            out <- urakka cols $ \cols' ->
                putStrs [ renderTable $ makeTable makeCell ghcs cols' ]

            return (renderTable . makeTable makeCell ghcs <$> stms, out)

        _ -> throwM (userError "no .cabal file found")

data Result
    = ResultPending
    | ResultOk      !Version
    | ResultAlmost  !Version
    | ResultNoPlan
    | ResultDepFail !Version
    | ResultFail    !Version
  deriving (Show, Generic)

instance NFData Result

makeCell ::  Result -> Txt
makeCell ResultPending     = mkTxt Black   "..."
makeCell (ResultOk v)      = mkTxt Green   $ prettyShow v
makeCell (ResultAlmost v)  = mkTxt Cyan    $ prettyShow v
makeCell ResultNoPlan      = mkTxt Blue    "no-plan"
makeCell (ResultDepFail v) = mkTxt Magenta $ prettyShow v
makeCell (ResultFail v)    = mkTxt Red     $ prettyShow v

boundsForGhc
    :: Verify
    -> Limit
    -> Path Absolute
    -> Map.Map PackageName (Set.Set Version)
    -> GenericPackageDescription
    -> GHCVer
    -> M (Map.Map PackageName (STM Result), Urakka () (Map.Map PackageName Result))
boundsForGhc verify limit dir index gpd ghcVer = do
    let deps = allBuildDepends (toVersion ghcVer) gpd
    let deps' = Map.intersectionWith withinRange' index deps

    resR <- for (Map.toList deps') $ \(pn, vs) -> do
        let f :: a -> (PackageName, a)
            f = (,) pn
        fmap (bimap f (fmap f)) $ findLowest pn $ case limit of
            LimitLower -> vs
            LimitUpper -> reverse vs

    let stms :: Map PackageName (STM Result)
        stms = Map.fromList $ map fst resR

        res :: Urakka () (Map PackageName Result)
        res = Map.fromList <$> traverse snd resR

    return (stms, res)
  where
    withinRange' vs vr = filter (`withinRange` vr) $ Set.toList vs

    findLowest :: PackageName -> [Version] -> M (STM Result, Urakka () Result)
    findLowest pn vs = do
        anyOk <- urakka (pure ()) $ \() -> do
            (ec, _, _) <- runCabal ModeDry dir ghcVer Map.empty
            return $ case ec of
                ExitSuccess   -> True
                ExitFailure _ -> False

        ur <- if_ anyOk <$> divideRanges majorVs <*> return (pure ResultNoPlan)
        (stm, post) <- urakkaSTM return
        return (stm, ur >>> post)
      where
        u = listToMaybe vs

        majorVs :: [NonEmpty Version]
        majorVs = NE.groupBy ((==) `on` extractMajorVersion) vs

        linear :: [Version] -> M (Urakka () Result)
        linear [] = return (pure ResultNoPlan)
        linear (v:vs') = do
            let constraints = Map.singleton pn (thisVersion v)

            uDry <- urakka' $ \() -> do
                (ec, _, _) <- runCabal ModeDry dir ghcVer constraints
                return $ case ec of
                    ExitSuccess   -> Right (resultOk u v)
                    ExitFailure _ -> Left ()

            -- uAll :: Urakka () (Either () Result)
            -- -- ^ either continue, or stop here with result
            uAll <- case verify of
                Verify ->  do
                    uDep <- urakka' $ \res -> do
                        (ec, _, _) <- runCabal ModeDep dir ghcVer constraints
                        if isFailure ec
                        then return $ Left (ResultDepFail v)
                        else return $ Right res

                    uBld <- urakka' $ \res -> do
                        (ec, _, _) <- runCabal ModeBuild dir ghcVer constraints
                        if isFailure ec
                        then return $ ResultFail v
                        else return res

                    return $ uDry >>> returnA +++ (uDep >>> returnA ||| uBld)

                SolveOnly -> return uDry

            rest <- linear vs'
            return $ uAll >>> rest ||| returnA

        linearRanges :: [NonEmpty Version] -> M (Urakka () Result)
        linearRanges [] = return (pure ResultNoPlan)
        linearRanges [r] = linear (NE.toList r)
        linearRanges (r:rs) = do
            inR <- urakka (pure ()) $ \() -> do
                (ec, _, _) <- runCabal ModeDry dir ghcVer $ Map.singleton pn $
                    intersectVersionRanges (orLaterVersion $ minimum r) (orEarlierVersion $ maximum r)
                return $ case ec of
                    ExitSuccess   -> True
                    ExitFailure _ -> False

            if_ inR <$> linear (NE.toList r) <*> linearRanges rs

        divideRanges :: [NonEmpty Version] -> M (Urakka () Result)
        divideRanges us = do
            case take3 us of
                Left us'         -> linearRanges us'
                Right (vsl, vsr) -> do
                    inLeft <- urakka (pure ()) $ \() -> do
                        (ec, _, _) <- runCabal ModeDry dir ghcVer $ Map.singleton pn $
                            intersectVersionRanges (orLaterVersion $ minimum2 vsl) (orEarlierVersion $ maximum2 vsl)
                        return $ case ec of
                            ExitSuccess   -> True
                            ExitFailure _ -> False

                    if_ inLeft <$> divideRanges (toList vsl) <*> divideRanges vsr

    isFailure (ExitFailure _) = True
    isFailure ExitSuccess     = False

    resultOk :: Maybe Version -> Version -> Result
    resultOk Nothing v  = ResultOk v
    resultOk (Just u) v
        | take 2 (versionNumbers u) == take 2 (versionNumbers v) = ResultOk v
        | otherwise = ResultAlmost v

take3 :: [a] -> Either [a] (NonEmpty a, [a])
take3 (a:b:c:d:e:fs) = Right (a :| [b,c,d], e:fs)
take3 xs           = Left xs

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

minimum2 :: (Foldable1 t, Foldable1 s, Ord a) => t (s a) -> a
minimum2 = getMin . foldMap1 (foldMap1 Min)

maximum2 :: (Foldable1 t, Foldable1 s, Ord a) => t (s a) -> a
maximum2 = getMax . foldMap1 (foldMap1 Max)

-------------------------------------------------------------------------------
-- Table
-------------------------------------------------------------------------------

makeTable :: (a -> Txt) -> [GHCVer] -> Map.Map (GHCVer, PackageName) a -> [[Txt]]
makeTable mkCell ghcs m
    = (emptyTxt : map (mkTxt Black . prettyShow. toVersion) ghcs)
    : map mkRow pns
  where
    pns = Set.toList $ Set.map snd $ Map.keysSet m

    mkRow :: PackageName -> [Txt]
    mkRow pn
        = mkTxt Black (prettyShow pn)
        : map (\g -> maybe emptyTxt mkCell $ Map.lookup (g, pn) m) ghcs
