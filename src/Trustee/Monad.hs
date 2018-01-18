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
import Data.Semigroup             (Semigroup (..))
import Data.Time                  (UTCTime, defaultTimeLocale, formatTime)
import Distribution.Package       (PackageName)
import Distribution.Text          (display)
import Distribution.Version
       (VersionRange, intersectVersionRanges, simplifyVersionRange)
import Foreign.C.Types            (CClock (..))
import Path                       (Abs, Dir, Path)
import System.Clock               (Clock (Monotonic), getTime, toNanoSecs)
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
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import qualified Path
import qualified System.Process           as Process

import Trustee.Config
import Trustee.GHC
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

data Env = Env
    { envConfig      :: Config
    , envIndexState  :: Maybe UTCTime
    , envConstraints :: Map PackageName VersionRange
    , envThreads     :: TVar Int
    , envGhcLocks    :: PerGHC (TVar Bool)
    , envTimeStats   :: TVar (Stats' (Average Double))
    , envRunStats    :: TVar Stats
    , envDoneStats   :: TVar Stats
    }

envCabalJobs :: Env -> Int
envCabalJobs = cfgCabalJobs . envConfig

envGhcJobs :: Env -> Int
envGhcJobs = cfgGhcJobs . envConfig

newEnv :: Config -> Maybe UTCTime -> Map PackageName VersionRange ->  IO Env
newEnv cfg is cons = atomically $ Env cfg is cons
    <$> newTVar (cfgThreads cfg)
    <*> sequence (pure (newTVar True))
    <*> newTVar (Stats mempty mempty mempty)
    <*> newTVar (Stats 0 0 0)
    <*> newTVar (Stats 0 0 0)

-- TODO: add lock here!
runM
    :: Config                         -- ^ configuration
    -> Maybe UTCTime                  -- ^ index state
    -> Map PackageName VersionRange   -- ^ constraints
    -> M a                            -- ^ action
    -> IO a
runM cfg is cons m = displayConsoleRegions $ withConsoleRegion Linear $ \region -> do
    env <- newEnv cfg is cons

    setConsoleRegion region $ do
        Stats dry dep build    <- readTVar (envDoneStats env)
        Stats dry' dep' build' <- readTVar (envRunStats env)
        -- TODO: learn this coefficients from data!
        -- Stats {statsDryRuns = "3.138", statsDepRuns = "0.151", statsBuildRuns = "84.700"
        let done = 3 * dry + 1 * dep + 85 * build
        -- dep coefficient is done's dep + build, because build is often
        -- blocked (not visible because of >>=)
        let run  = 3 * dry' + 86 * dep' + 85 * build'
        return $ T.pack $ concat
            [ "dry: "
            , show dry
            , ";  dep: "
            , show dep
            , ";  bld: "
            , show build
            , ";  queue: "
            , show (dry' + dep' + build')
            , ";  done: "
            , printf "%.02f" $ (100 :: Double) *
                fromIntegral done / fromIntegral (done + run)
            , "%"
            ]

    startTime <- getTime Monotonic
    x <- runM' env m
    endTime <- getTime Monotonic

    -- Print stats
    processTimes <- getProcessTimes
    stats <- readTVarIO (envDoneStats env)
    timeStats <- readTVarIO (envTimeStats env)

    let formatStats f (Stats dry dep bld) = concat
            [ "dry: " ++ f dry
            , "   dep: " ++ f dep
            , "   bld: " ++ f bld
            , "\n"
            ] :: String

    outputConcurrent $ formatStats (printf "%6d ") stats
    outputConcurrent $ formatStats (printf "%6.03fs" . getAverage) timeStats
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
    | ModeBuild'
  deriving (Eq, Ord, Enum, Bounded, Show)

toMode' :: Mode -> Mode'
toMode' ModeDry       = ModeDry'
toMode' ModeDryTest   = ModeDry'
toMode' ModeDep       = ModeDep'
toMode' ModeDepTest   = ModeDep'
toMode' ModeBuild     = ModeBuild'
toMode' ModeBuildTest = ModeBuild'

findPlan :: Path Abs Dir -> GHCVer -> Map PackageName VersionRange -> M Cabal.PlanJson
findPlan dir ghcVersion constraints = liftIO $ fmap fst $
    Cabal.findAndDecodePlanJson (Just dir')
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

    (_jGHC, jCabal) <- jobs
    runWithGHC mode' dir ghcVersion "cabal" $
        [ "new-build"
        , "--builddir=.dist-newstyle-" ++ buildDirSuffix constraintsS
        , "-w", "ghc-" ++ ghcVersion'
        , testFlag, "--disable-benchmarks"
        , "-j" ++ show jCabal
        -- , "--ghc-options=" ++ ghcOptions
        ] ++ modeArg ++ indexStateArg ++ constraintsArg ++
        [ "all"
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
        ModeBuild'     -> []
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
        ModeBuild' -> s { statsBuildRuns = f (statsBuildRuns s) }
        ModeDep'   -> s { statsDepRuns   = f (statsDepRuns s) }
        ModeDry'   -> s { statsDryRuns   = f (statsDryRuns s) }

    action env =  bracket acquire release $ \() -> do
        outputConcurrent $ dir' ++ " " ++ colored Blue formatted ++ "\n"
        let process = (Process.proc cmd args) { Process.cwd = Just $ Path.toFilePath dir }
        startTime <- getTime Monotonic
        (ec, o', e') <- Process.readCreateProcessWithExitCode process ""
        endTime <- getTime Monotonic
        let diff = fromInteger (toNanoSecs (endTime - startTime)) / 1e9 :: Double
        atomically $ modifyTVar' (envTimeStats env) (modifyStats (mkAverage diff <>))
        outputConcurrent $  dir' ++ " " ++ colored (color ec) formatted ++ printf " %.03fs" diff ++ "\n"
        o <- evaluate (force o')
        e <- evaluate (force e')
        return (ec, o, e)
      where
        threads
            | dry = 1
            | otherwise = ghcJobs (envGhcJobs env) ghcVersion * envCabalJobs env

        ghcLock = index (envGhcLocks env) ghcVersion
        threadLock = envThreads env

        acquire = do
            atomically $ do
                unless dry $ do
                    b <- readTVar ghcLock
                    unless b retry
                    writeTVar ghcLock False

                n <- readTVar threadLock
                unless (n >= threads) retry

                let n' = n - threads
                writeTVar threadLock $! n'

        release _ = atomically $ do
            unless dry $ writeTVar ghcLock True
            n <- readTVar threadLock
            writeTVar threadLock $! n + threads

jobs :: M (Int, Int)
jobs = mkM $ \env -> pure (envGhcJobs env, envCabalJobs env)

askConstraints :: Map PackageName VersionRange -> M (Map PackageName VersionRange)
askConstraints c = mkM $ \env -> pure
    $ Map.unionWith intersectVersionRanges c
    $ envConstraints env

askIndexState :: M (Maybe UTCTime)
askIndexState = mkM $ return . envIndexState

-------------------------------------------------------------------------------
-- Average
-------------------------------------------------------------------------------

-- | Numerically stable average. Weighted average to be precise.
--
-- >>> getAverage $ foldMap mkAverage [1,2,4,7] :: Rational
-- 7 % 2
data Average a = Average { _samples :: !a, getAverage :: !a }
  deriving (Eq, Show)

-- | Make 'Average' with 1 as a sample size.
mkAverage :: Num a => a -> Average a
mkAverage x = Average 1 x

instance (Eq a, Fractional a) => Semigroup (Average a) where
    a@(Average n x) <> b@(Average n' x')
        | n == 0    = b
        | n' == 0   = a
        | otherwise = Average m y
      where
        m = n + n'
        y = (n * x + n' * x') / m

instance (Eq a, Fractional a) => Monoid (Average a) where
    mempty = Average 0 0
    mappend = (<>)
