{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Trustee.Monad (
    M,
    runM,
    putStrs,
    forConcurrently,
    runCabal,
    findPlan,
    Mode (..),
    Mode' (..),
    -- * Internal
    runWithGHC,
    jobs,
    ) where

import Control.Concurrent.STM
       (TVar, atomically, modifyTVar', newTVar, readTVar, readTVarIO, retry,
       writeTVar)
import Control.DeepSeq            (force)
import Control.Exception          (bracket, evaluate)
import Control.Monad              (unless)
import Control.Monad.IO.Class     (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.List                  (isPrefixOf)
import Data.Map                   (Map)
import Data.Maybe                 (fromMaybe)
import Data.Time
       (UTCTime, addUTCTime, defaultTimeLocale, formatTime, getCurrentTime,
       getCurrentTimeZone, utcToLocalTime)
import Distribution.Package       (PackageName)
import Distribution.Text          (display)
import Distribution.Version
       (VersionRange, intersectVersionRanges, simplifyVersionRange)
import Foreign.C.Types            (CClock (..))
import Path                       (Abs, Dir, Path)
import System.Clock
       (Clock (Monotonic), TimeSpec, getTime, toNanoSecs)
import System.Console.Concurrent  (outputConcurrent)
import System.Console.Regions
       (RegionLayout (Linear), displayConsoleRegions, setConsoleRegion,
       withConsoleRegion)
import System.Exit                (ExitCode (..))
import System.FilePath            ((</>))
import System.Posix.Process       (ProcessTimes (..), getProcessTimes)
import Text.Printf                (printf)

import qualified Cabal.Plan               as Cabal
import qualified Control.Concurrent.Async as Async
import qualified Crypto.Hash.SHA512       as SHA512
import qualified Data.Binary              as Binary
import qualified Data.ByteString.Base16   as BS16
import qualified Data.Map.Strict          as Map
import qualified Data.TDigest             as TD
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import qualified Path
import qualified System.Process           as Process

import Trustee.Config
import Trustee.GHC
import Trustee.Lock
import Trustee.Options
import Trustee.Table
import Trustee.Txt

newtype M a = M { unM :: ReaderT Env IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

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
    { envConfig      :: Config
    , envPlanParams  :: PlanParams
    , envThreads     :: TVar Int
    , envGhcLocks    :: PerGHC (TVar (Maybe GHCLock))
    , envClock       :: TVar TimeSpec
    , envTimeStats   :: TVar (Stats' (TD.TDigest 25))
    , envTimeSums    :: TVar (Stats' Double)
    , envRunStats    :: TVar Stats
    , envDoneStats   :: TVar Stats
    }

envCabalJobs :: Env -> Int
envCabalJobs = cfgCabalJobs . envConfig

envGhcJobs :: Env -> Int
envGhcJobs = cfgGhcJobs . envConfig

newEnv :: Config -> PlanParams -> TimeSpec -> IO Env
newEnv cfg pp ts = atomically $ Env cfg pp
    <$> newTVar (cfgThreads cfg)
    <*> sequence (pure (newTVar Nothing))
    <*> newTVar ts
    <*> newTVar (Stats mempty mempty mempty)
    <*> newTVar (Stats 0 0 0)
    <*> newTVar (Stats 0 0 0)
    <*> newTVar (Stats 0 0 0)

runM
    :: Config      -- ^ configuration
    -> PlanParams  -- ^ plan configuration
    -> M a         -- ^ action
    -> IO a
runM cfg pp m = withLock $ displayConsoleRegions $ withConsoleRegion Linear $ \region -> do
    -- Start times
    tz           <- getCurrentTimeZone
    startUtcTime <- getCurrentTime
    startTime    <- getTime Monotonic

    -- Environment
    env <- newEnv cfg pp startTime

    -- Stats region
    setConsoleRegion region $ do
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

    x <- runM' env m
    endTime <- getTime Monotonic

    -- Print stats
    processTimes <- getProcessTimes
    stats        <- readTVarIO (envDoneStats env)
    timeStats    <- readTVarIO (envTimeStats env)
    timeSums     <- readTVarIO (envTimeSums env)

    let formatStats header f (Stats dry dep bld) = map (mkTxt Black) $
            header : [ f dry , f dep , f bld ]

    outputConcurrent $ renderTable
        [ [ emptyTxt, mkTxt Black "dry", mkTxt Black "dep", mkTxt Black "build" ]
        , formatStats "counts"  (printf "%d ") stats
        , formatStats "q50"     (printf "%.03fs" . fromMaybe 0 . TD.quantile 0.5) timeStats
        , formatStats "q90"     (printf "%.03fs" . fromMaybe 0 . TD.quantile 0.9) timeStats
        , formatStats "total"   (printf "%.03fs") timeSums
        ]

    outputConcurrent $
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

mkM :: (Env -> IO a) -> M a
mkM = M . ReaderT

runM' :: Env -> M a -> IO a
runM' env m = runReaderT (unM m) env

putStrs :: [String] -> M ()
putStrs = liftIO . outputConcurrent . unlines

forConcurrently :: Traversable t => t a -> (a -> M b) -> M (t b)
forConcurrently xs f = mkM $ \env ->
    Async.forConcurrently xs (runM' env . f)

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

findPlan :: Path Abs Dir -> GHCVer -> Map PackageName VersionRange -> M Cabal.PlanJson
findPlan dir ghcVersion constraints = liftIO $
    Cabal.findAndDecodePlanJson (Cabal.InBuildDir dir')
  where
    dir' = Path.toFilePath dir </> (".dist-newstyle-" ++ buildDirSuffix)
    constraintsS = fmap simplifyVersionRange constraints
    buildDirSuffix
        = T.unpack
        $ TE.decodeUtf8
        $ BS16.encode
        $ SHA512.hashlazy
        $ Binary.encode (ghcVersion, constraintsS)

runCabal :: Mode -> Path Abs Dir -> GHCVer -> Map PackageName VersionRange -> M (ExitCode, String, String)
runCabal mode dir ghcVersion constraints = do
    constraints' <- askConstraints constraints
    let constraintsS = fmap simplifyVersionRange constraints'
    let constraintsArg = uncurry mkConstraint <$> Map.toList constraintsS

    indexState <- askIndexState
    let indexStateArg = maybe [] (return . formatTime defaultTimeLocale "--index-state=%Y-%m-%dT%H:%M:%SZ") indexState

    allowNewer <- askAllowNewer
    let allowNewerArg = map ("--allow-newer=" ++) allowNewer

    backjumps <- askBackjumps
    let backjumpsArg = maybe [] (\b -> ["--max-backjumps=" ++ show b]) backjumps

    (_jGHC, jCabal) <- jobs
    runWithGHC mode' dir ghcVersion "cabal" $
        [ "new-build"
        , "--builddir=.dist-newstyle-" ++ buildDirSuffix constraintsS
        , "-w", "ghc-" ++ ghcVersion'
        , testFlag, "--disable-benchmarks"
        , "-j" ++ show jCabal
        -- , "--ghc-options=" ++ ghcOptions
        ] ++ modeArg ++ indexStateArg ++ backjumpsArg ++ allowNewerArg ++ constraintsArg ++
        [ "."
        ]
  where
    test = case mode of
        ModeDryTest   -> True
        ModeDepTest   -> True
        ModeBuildTest -> True
        _             -> False

    testFlag
        | test      = "--enable-tests"
        | otherwise = "--disable-tests"

    ghcVersion' = display (toVersion ghcVersion)
    mode' = toMode' mode
    modeArg = case mode' of
        ModeDry'       -> ["--dry-run"]
        ModeDep'       -> ["--dep"]
        ModeBld'     -> []
    mkConstraint pkgName vr = "--constraint=" ++ display pkgName ++ display vr

    buildDirSuffix constraintsS
        = T.unpack
        $ TE.decodeUtf8
        $ BS16.encode
        $ SHA512.hashlazy
        $ Binary.encode (ghcVersion, constraintsS)

runWithGHC :: Mode' -> Path Abs Dir -> GHCVer -> FilePath -> [String] -> M (ExitCode, String, String)
runWithGHC mode dir ghcVersion cmd args = mkM putRun *> mkM action <* mkM putStats
  where
    dry = mode == ModeDry'
    dir' = Path.toFilePath $ Path.dirname dir
    formatted = cmd ++ " " ++ unwords args'
    -- TODO: verbosity flag to show all
    args' = take 10 $ filter (not . isPrefixOf "--builddir=") args
    color ExitSuccess     = if dry then Cyan else Green
    color (ExitFailure _) = if dry then Magenta else Red

    putRun   env = atomically $ modifyTVar' (envRunStats env) (modifyStats succ)
    putStats env = atomically $ do
        modifyTVar' (envRunStats env) (modifyStats pred)
        modifyTVar' (envDoneStats env) (modifyStats succ)

    modifyStats f s = case mode of
        ModeBld' -> s { statsBuildRuns = f (statsBuildRuns s) }
        ModeDep'   -> s { statsDepRuns   = f (statsDepRuns s) }
        ModeDry'   -> s { statsDryRuns   = f (statsDryRuns s) }

    action env =  bracket acquire release $ \() -> do
        outputConcurrent $ dir' ++ " " ++ colored Blue formatted ++ "\n"
        let process = (Process.proc cmd args) { Process.cwd = Just $ Path.toFilePath dir }
        startTime <- getTime Monotonic
        (ec, o', e') <- Process.readCreateProcessWithExitCode process ""
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
      where
        threads = case mode of
            ModeDry'   -> 1
            ModeDep'   -> envCabalJobs env
            ModeBld' -> ghcJobs (envGhcJobs env) ghcVersion

        ghcLock = index (envGhcLocks env) ghcVersion
        threadLock = envThreads env

        acquire = atomically $ do
            case mode of
                ModeDry' -> pure ()
                ModeDep' -> do
                    b <- readTVar ghcLock
                    case b of
                        Nothing       -> writeTVar ghcLock (Just GHCStore)
                        Just GHCStore -> pure ()
                        _             -> retry
                ModeBld' -> do
                    b <- readTVar ghcLock
                    case b of
                        Nothing           -> writeTVar ghcLock (Just (GHCBuild 0))
                        Just (GHCBuild n) -> writeTVar ghcLock (Just (GHCBuild (succ n)))
                        _                 -> retry

            n <- readTVar threadLock
            unless (n >= threads) retry

            let n' = n - threads
            writeTVar threadLock $! n'

        release _ = atomically $ do
            case mode of
                ModeDry' -> pure ()
                ModeDep' -> do
                    b <- readTVar ghcLock
                    case b of
                        Nothing       -> pure () -- weird
                        Just GHCStore -> writeTVar ghcLock Nothing
                        _             -> retry
                ModeBld' -> do
                    b <- readTVar ghcLock
                    case b of
                        Nothing         -> pure () -- weird
                        Just (GHCBuild n)
                            | n <= 0    -> writeTVar ghcLock Nothing
                            | otherwise -> writeTVar ghcLock (Just (GHCBuild (pred n)))
                        _               -> retry
            n <- readTVar threadLock
            writeTVar threadLock $! n + threads

jobs :: M (Int, Int)
jobs = mkM $ \env -> pure (envGhcJobs env, envCabalJobs env)

askConstraints :: Map PackageName VersionRange -> M (Map PackageName VersionRange)
askConstraints c = mkM $ \env -> pure
    $ Map.unionWith intersectVersionRanges c
    $ ppConstraints $ envPlanParams env

askIndexState :: M (Maybe UTCTime)
askIndexState = mkM $ return . ppIndexState . envPlanParams

askAllowNewer :: M [String]
askAllowNewer = mkM $ return . ppAllowNewer . envPlanParams

askBackjumps :: M (Maybe Int)
askBackjumps = mkM $ return . ppBackjumps . envPlanParams
