{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
-- {-# OPTIONS_GHC -Werror #-}
module Trustee.Monad (
    M,
    runM,
    runUrakkaM,
    putStrs,
    runCabal,
    findPlan,
    Mode (..),
    Mode' (..),
    Env,
    -- * Internal
    runWithGHC,
    ) where

import Control.Concurrent.STM
       (STM, TVar, atomically, modifyTVar, modifyTVar', newTVarIO, readTVar, readTVarIO, retry, writeTVar)
import Control.Exception          (evaluate)
import Data.Functor.Representable (index)
import Data.List                  (isPrefixOf)
import Data.Monoid                (Sum (..))
import Data.Time
       (UTCTime, addUTCTime, defaultTimeLocale, formatTime, getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import Distribution.Types.Flag    (dispFlagAssignment)
import Distribution.Version       (VersionRange, anyVersion, simplifyVersionRange)
import Foreign.C.Types            (CClock (..))
import System.Clock               (Clock (Monotonic), TimeSpec, getTime, toNanoSecs)
import System.Console.Concurrent  (outputConcurrent)
import System.Console.Regions     (RegionLayout (Linear), setConsoleRegion, withConsoleRegion)
import System.Exit                (ExitCode (..))
import System.Posix.Process       (ProcessTimes (..), getProcessTimes)
import Text.Printf                (printf)
import Urakka
       (ConcSt, Urakka, overEstimate, runConcurrent', underEstimate, urakkaDone, urakkaOverEstimate, urakkaQueued)
import Urakka.Estimation          (addEstimationPoint, currentEstimate, mkEstimator)

import qualified Cabal.Plan               as Cabal
import qualified Control.Concurrent.Async as Async
import qualified Crypto.Hash.SHA512       as SHA512
import qualified Data.Binary              as Binary
import qualified Data.ByteString.Base16   as BS16
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.Map.Strict          as Map
import qualified Data.TDigest             as TD
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import qualified System.Path              as Path
import qualified Text.PrettyPrint         as PP

import Trustee.Config
import Trustee.GHC
import Trustee.Lock
import Trustee.Options
import Trustee.Table
import Trustee.Txt

import Peura          hiding (evaluate)
import VendingMachine

type M = Peu Env

data Stats' a = Stats
    { statsDryRuns   :: !a
    , statsDepRuns   :: !a
    , statsBuildRuns :: !a
    }
  deriving (Functor, Show)

type Stats = Stats' Int

data GHCLock
    = GHCStore       -- ^ dependencies, exclusive (writes to store)
    | GHCBuild !Int  -- ^ build,        shared (only reads store)

data Env = Env
    { envConfig         :: Config
    , envPlanParams     :: PlanParams
    , envThreads        :: TVar Int
    , envGhcLocks       :: PerGHC (TVar (Maybe GHCLock))
    , envClock          :: TVar TimeSpec
    , envTimeStats      :: TVar (Stats' (TD.TDigest 25))
    , envTimeSums       :: TVar (Stats' Double)
    , envRunStats       :: TVar Stats
    , envDoneStats      :: TVar Stats
    , envVendingMachine :: VendingMachine L
    }

data L a where
    LDry :: L ()
    LDep :: GHCVer -> L ()
    LBld :: GHCVer -> L ()

envCabalJobs :: Env -> Int
envCabalJobs = cfgCabalJobs . envConfig

envGhcJobs :: Env -> Int
envGhcJobs = cfgGhcJobs . envConfig

newEnv :: Config -> PlanParams -> TimeSpec -> IO (Env, IO ())
newEnv cfg pp ts = do
    let envConfig     = cfg
        envPlanParams = pp

    envThreads   <- newTVarIO (cfgThreads cfg)
    envGhcLocks  <- sequenceA (pure (newTVarIO Nothing))
    envClock     <- newTVarIO ts
    envTimeStats <- newTVarIO (Stats mempty mempty mempty)
    envTimeSums  <- newTVarIO (Stats 0 0 0)
    envRunStats  <- newTVarIO (Stats 0 0 0)
    envDoneStats <- newTVarIO (Stats 0 0 0)

    -- we prefer to build dependencies early, then build packages, and then solve.
    let price :: L a -> Sum Int
        price (LDep _) = 0
        price (LBld _) = 1
        price LDry     = 2

    let supply :: L a -> STM (a, STM ())
        supply LDry = do
            let threads = 1
            n <- readTVar envThreads
            unless (n >= threads) retry
            writeTVar envThreads $ n - threads

            return ((), modifyTVar envThreads (+ threads))

        supply (LDep gv) = do
            let threads = cfgCabalJobs envConfig
            let ghcLock = index envGhcLocks gv

            n <- readTVar envThreads
            -- start dep if there's one slot, i.e. over-provision
            unless (n >= 1) retry
            writeTVar envThreads $ n - threads

            b <- readTVar ghcLock
            case b of
                Nothing       -> writeTVar ghcLock (Just GHCStore)
                Just _        -> retry

            let release :: STM ()
                release = do
                    modifyTVar envThreads (+ threads)
                    modifyTVar ghcLock $ \case
                        Just GHCStore -> Nothing
                        x             -> x

            return ((), release)

        supply (LBld gv) = do
            let threads = cfgGhcJobs envConfig
            let ghcLock = index envGhcLocks gv

            n <- readTVar envThreads
            unless (n >= threads) retry
            writeTVar envThreads $ n - threads

            b <- readTVar ghcLock
            case b of
                Nothing           -> writeTVar ghcLock (Just (GHCBuild 0))
                Just (GHCBuild m) -> writeTVar ghcLock (Just (GHCBuild (succ m)))
                Just _            -> retry

            let release :: STM ()
                release = do
                    modifyTVar envThreads (+ threads)
                    modifyTVar ghcLock $ \case
                        Just (GHCBuild m)
                            | m <= 0    -> Nothing
                            | otherwise -> Just (GHCBuild (m - 1))
                        x             -> x

            return ((), release)

    (envVendingMachine, stop) <- makeVendingMachine supply price

    return (Env {..}, stop)

runM
    :: Config      -- ^ configuration
    -> PlanParams  -- ^ plan configuration
    -> M a         -- ^ action
    -> Peu () a
runM cfg pp m = withRunInIO $ \runInIO -> withLock $ runInIO $ withConsoleRegion Linear $ \region -> do
    -- Start times
    tz           <- liftIO getCurrentTimeZone
    startUtcTime <- liftIO getCurrentTime
    startTime    <- liftIO $ getTime Monotonic

    -- Environment
    (env, stop) <- liftIO $ newEnv cfg pp startTime

    -- Stats region
    liftIO $ setConsoleRegion region $ do
        Stats dryT depT bldT <- readTVar (envTimeStats env)
        Stats dryW depW bldW <- readTVar (envTimeSums env)
        Stats dryD depD bldD <- readTVar (envDoneStats env)
        Stats dryR depR bldR <- readTVar (envRunStats env)

        let dryK = fromMaybe   2 $ TD.quantile 0.6 dryT
        let depK = fromMaybe 120 $ TD.quantile 0.6 depT
        let bldK = fromMaybe  60 $ TD.quantile 0.6 bldT

        let withK k x = k * fromIntegral x

        let done  = dryW + depW + bldW
        let done' = withK dryK dryD + withK depK depD + withK bldK bldD
        -- dep coefficient is done's dep + build, because build is often
        -- blocked (not visible because of >>=)
        let run  = withK (dryK + depK + bldK) dryR + withK (depK + bldK) depR + withK bldK bldR

        -- completion percent
        let percent = done / (done + run)

        -- ETA
        currTime <- readTVar (envClock env)
        let diff = fromIntegral (toNanoSecs (currTime - startTime)) / (1e9 :: Double)
        let esti = diff / percent
        let eta  = utcToLocalTime tz $ addUTCTime (realToFrac esti) startUtcTime

        return $ T.pack $ concat
            [ "dry: "
            , show dryD
            , "/"
            , show (dryD + dryR)
            , "   dep: "
            , show depD
            , "/"
            , show (depD + depR)
            , "   bld: "
            , show bldD
            , "/"
            , show (bldD + bldR)
            , "   done: "
            , printf "%.02f" $ 100 * percent
            , "%"
            , "   eta: "
            , formatTime defaultTimeLocale "%T" eta
            , "  dbg: " ++ show (done, done')
            ]

    x <- changePeu (\() -> env) m
    endTime <- liftIO $ getTime Monotonic

    -- stop vending machine
    liftIO stop

    -- Print stats
    processTimes <- liftIO $ getProcessTimes
    stats        <- liftIO $ readTVarIO (envDoneStats env)
    timeStats    <- liftIO $ readTVarIO (envTimeStats env)
    timeSums     <- liftIO $ readTVarIO (envTimeSums env)

    let formatStats header f (Stats dry dep bld) = map (mkTxt Black) $
            header : [ f dry , f dep , f bld ]

    liftIO $ outputConcurrent $ renderTable
        [ [ emptyTxt, mkTxt Black "dry", mkTxt Black "dep", mkTxt Black "build" ]
        , formatStats "counts"  (printf "%d ") stats
        , formatStats "q50"     (printf "%.03fs" . fromMaybe 0 . TD.quantile 0.5) timeStats
        , formatStats "q90"     (printf "%.03fs" . fromMaybe 0 . TD.quantile 0.9) timeStats
        , formatStats "total"   (printf "%.03fs") timeSums
        ]

    liftIO $ outputConcurrent $
        let ProcessTimes _ _ _ (CClock u') (CClock s') = processTimes
            e = fromInteger (toNanoSecs (endTime - startTime)) / 1e9 :: Double
            u = fromIntegral u' / 1e2 :: Double
            s = fromIntegral s' / 1e2 :: Double
        in concat
            [ printf "usr: %6.03fs   " u
            , printf "sys: %6.03fs   " s
            , printf "cpu: %6.03f%%   " (100 * (u + s) / e)
            , printf "time: %.03fs\n" e
            ] :: String


    return x

runUrakkaM :: M (STM String, Urakka () a) -> M a
runUrakkaM actionU = do
    (progress, u) <- actionU
    withRunInIO $ \_runInIO ->
        withConsoleRegion Linear $ \region -> do
        withConsoleRegion Linear $ \regionStatusLine -> do
            estimator <- mkEstimator

            let mi = underEstimate u
            let ma = overEstimate u

            tz           <- getCurrentTimeZone
            startUtcTime <- getCurrentTime

            let onDone :: ConcSt -> IO ()
                onDone = addEstimationPoint estimator

            let urakkaLine :: ConcSt -> STM String
                urakkaLine concSt = do
                    q <- urakkaQueued concSt
                    d <- urakkaDone concSt
                    ma' <- urakkaOverEstimate concSt

                    dur <- currentEstimate estimator
                    let eta = fmap (\dur' -> utcToLocalTime tz $ addUTCTime (realToFrac dur') startUtcTime) dur

                    return $ unwords
                        [ "trustee "
                        , show d
                        , "/"
                        , show (max mi q)
                        , "(" ++ show ma' ++ "/" ++ show ma ++ ")"
                        -- , maybe "est. dur:" (printf "%.02fs") dur
                        , maybe "no ETA" (formatTime defaultTimeLocale "ETA %T") eta
                        , show dur
                        ]

            (asyncUrakka, concSt) <- runConcurrent' onDone () u

    {-
            titleThread <- async $ do
                let loop :: Maybe String -> IO ()
                    loop old = do
                        title <- atomically $ do
                            new <- urakkaLine concSt
                            when (Just new == old) retry
                            return new

                        setTitle title
                        loop (Just title)

                loop Nothing
    -}

            setConsoleRegion region (fmap T.pack progress)

            setConsoleRegion regionStatusLine $ fmap T.pack $ urakkaLine concSt
            result <- Async.wait asyncUrakka
            return result

mkM :: (Env -> IO a) -> M a
mkM f = ask >>= liftIO . f

putStrs :: MonadIO m => [String] -> m ()
putStrs = liftIO . outputConcurrent . unlines

data Mode
    = ModeDry
    | ModeDep
    | ModeBuild
    | ModeDryTest
    | ModeDepTest
    | ModeBuildTest
  deriving (Eq, Ord, Enum, Bounded, Show)

data Mode'
    = ModeDry'
    | ModeDep'
    | ModeBld'
  deriving (Eq, Ord, Enum, Bounded, Show)

toMode' :: Mode -> Mode'
toMode' ModeDry       = ModeDry'
toMode' ModeDryTest   = ModeDry'
toMode' ModeDep       = ModeDep'
toMode' ModeDepTest   = ModeDep'
toMode' ModeBuild     = ModeBld'
toMode' ModeBuildTest = ModeBld'

findPlan :: Path Absolute -> GHCVer -> Map PackageName VersionRange -> M Cabal.PlanJson
findPlan dir ghcVersion constraints = liftIO $
    Cabal.findAndDecodePlanJson (Cabal.InBuildDir dir')
  where
    dir' = Path.toFilePath $ dir </> fromUnrootedFilePath (".dist-newstyle-trustee/" ++ buildDirSuffix)
    constraintsS = fmap simplifyVersionRange constraints
    buildDirSuffix
        = T.unpack
        $ TE.decodeUtf8
        $ BS16.encode
        $ SHA512.hashlazy
        $ Binary.encode (ghcVersion, constraintsS)

runCabal :: Mode -> Path Absolute -> GHCVer -> Map PackageName VersionRange -> M (ExitCode, ByteString, ByteString)
runCabal mode dir ghcVersion constraints = do
    constraints' <- askConstraints constraints
    let constraintsS = fmap simplifyVersionRangeC constraints'
    let constraintsArgs = concatMap (uncurry mkConstraints) (Map.toList constraintsS)

    indexState <- askIndexState
    let indexStateArg = maybe [] (return . formatTime defaultTimeLocale "--index-state=%Y-%m-%dT%H:%M:%SZ") indexState

    allowNewer <- askAllowNewer
    let allowNewerArg = map ("--allow-newer=" ++) allowNewer

    backjumps <- askBackjumps
    let backjumpsArg = maybe [] (\b -> ["--max-backjumps=" ++ show b]) backjumps

    repos <- askLocalRepos
    let reposArgs :: [String]
        reposArgs =
            [ "--local-no-index-repo=" ++ name ++ ":" ++ toFilePath repoPath
            | repoPath <- repos
            , let name = toUnrootedFilePath (takeFileName repoPath)
            ]

    (_jGHC, jCabal) <- jobs
    (ec, out, err) <- runWithGHC mode' dir ghcVersion "cabal" $
        reposArgs ++
        [ "new-build"
        , "--builddir=.dist-newstyle-trustee/" ++ buildDirSuffix constraintsS
        , "-w", "ghc-" ++ ghcVersion'
        , testFlag, "--disable-benchmarks"
        , "-j" ++ show jCabal
        -- , "--ghc-options=" ++ ghcOptions
        ] ++ optArg ++ modeArg ++ indexStateArg ++ backjumpsArg ++ allowNewerArg ++ constraintsArgs ++
        [ "."
        ]

    return (ec, LBS.toStrict out, LBS.toStrict err)
  where
    optArg = case mode of
        ModeBuild -> ["-O0"]
        _         -> []

    test = case mode of
        ModeDryTest   -> True
        ModeDepTest   -> True
        ModeBuildTest -> True
        _             -> False

    testFlag
        | test      = "--enable-tests"
        | otherwise = "--disable-tests"

    ghcVersion' = prettyShow (toVersion ghcVersion)
    mode' = toMode' mode
    modeArg = case mode' of
        ModeDry'       -> ["--dry-run"]
        ModeDep'       -> ["--dep"]
        ModeBld'     -> []

    mkConstraints pkgName (PackageConstraint vr fa) =
        [ "--constraint=" ++ prettyShow pkgName ++ prettyShow vr | vr /= anyVersion ] ++
        -- TODO: Cabal-3.4
        [ "--constraint=" ++ prettyShow pkgName ++ PP.render (dispFlagAssignment fa) | fa /= mempty ]

    buildDirSuffix constraintsS
        = T.unpack
        $ TE.decodeUtf8
        $ BS16.encode
        $ SHA512.hashlazy
        $ Binary.encode (ghcVersion, constraintsS)

runWithGHC :: Mode' -> Path Absolute -> GHCVer -> FilePath -> [String] -> M (ExitCode, LBS.ByteString, LBS.ByteString)
runWithGHC mode dir ghcVersion cmd args = do
    env <- ask
    liftIO $ putRun env
    x <- action env
    liftIO $ putStats env
    return x
  where
    dry = mode == ModeDry'
    dir' = Path.toUnrootedFilePath $ Path.takeFileName dir
    formatted = cmd ++ " " ++ unwords args'
    -- TODO: verbosity flag to show all
    args' = take 10 $ filter (not . isPrefixOf "--builddir=") args
    color ExitSuccess     = if dry then Cyan else Green
    color (ExitFailure _) = if dry then Magenta else Red

    putRun   env = atomically $ modifyTVar' (envRunStats env) (modifyStats succ)
    putStats env = atomically $ do
        modifyTVar' (envRunStats env) (modifyStats pred)
        modifyTVar' (envDoneStats env) (modifyStats succ)

    modifyStats :: (a -> a) -> Stats' a -> Stats' a
    modifyStats f s = case mode of
        ModeBld' -> s { statsBuildRuns = f (statsBuildRuns s) }
        ModeDep'   -> s { statsDepRuns   = f (statsDepRuns s) }
        ModeDry'   -> s { statsDryRuns   = f (statsDryRuns s) }

    wishes :: Wishes L ()
    wishes = case mode of
        ModeDry' -> wish LDry
        ModeDep' -> wish (LDep ghcVersion)
        ModeBld' -> wish (LBld ghcVersion)

    action :: Env -> M (ExitCode, LBS.ByteString, LBS.ByteString)
    action env = withWishes (envVendingMachine env) wishes $ \() -> do
        startTime <- liftIO $ do
            outputConcurrent $ dir' ++ " " ++ colored Blue formatted ++ "\n"
            getTime Monotonic
        (ec, o', e') <- runProcess nullTracer' dir cmd args mempty
        liftIO $ do
            endTime <- getTime Monotonic
            let diff = fromInteger (toNanoSecs (endTime - startTime)) / 1e9 :: Double
            atomically $ do
                modifyTVar' (envTimeSums env)  (modifyStats (+ diff))
                modifyTVar' (envTimeStats env) (modifyStats (TD.insert diff))
                writeTVar (envClock env) endTime
            outputConcurrent $  dir' ++ " " ++ colored (color ec) formatted ++ printf " %.03fs" diff ++ "\n"
            o <- evaluate (force o')
            e <- evaluate (force e')
            return (ec, o, e)

    nullTracer' = nullTracer :: Tracer M TraceProcess

jobs :: M (Int, Int)
jobs = mkM $ \env -> pure (envGhcJobs env, envCabalJobs env)

askConstraints :: Map PackageName VersionRange -> M (Map PackageName PackageConstraint)
askConstraints c = mkM $ \env -> pure
    $ Map.unionWith (<>) (fmap (`PackageConstraint` mempty) c)
    $ ppConstraints $ envPlanParams env
  where

askIndexState :: M (Maybe UTCTime)
askIndexState = mkM $ return . ppIndexState . envPlanParams

askAllowNewer :: M [String]
askAllowNewer = mkM $ return . ppAllowNewer . envPlanParams

askBackjumps :: M (Maybe Int)
askBackjumps = mkM $ return . ppBackjumps . envPlanParams

askLocalRepos :: M [Path Absolute]
askLocalRepos = mkM $ return . ppLocalRepos . envPlanParams

simplifyVersionRangeC :: PackageConstraint -> PackageConstraint
simplifyVersionRangeC (PackageConstraint vr fa) = PackageConstraint (simplifyVersionRange vr) fa
