{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Peura.Monad (
    Peu (..),
    runPeu,
    changePeu,
    -- * Output
    withSetSgrCode,
    output,
    -- * Diagnostics
    putDebug,
    putInfo,
    putWarning,
    putError,
    -- ** Warning class
    Warning (..),
    -- * Control.Exception
    evaluate,
    evaluateForce,
    -- * System.Environment
    getArgs,
    lookupEnv,
    -- * System.Exit
    exitFailure,
    ) where

import System.Console.Concurrent (withConcurrentOutput, errorConcurrent, outputConcurrent)
import Control.Monad.IO.Class    (MonadIO (..))
import Control.Monad.Reader.Class (MonadReader (..))
import Peura.Exports
import Prelude                   (Double, Fractional (..), fromIntegral)
import System.Clock
       (Clock (Monotonic), TimeSpec (TimeSpec), diffTimeSpec, getTime)
import System.IO                 (hPutStrLn, stderr)
import Text.Printf               (printf)

import qualified Control.Exception   as X
import qualified System.Console.ANSI as ANSI
import qualified System.Environment  as X
import qualified System.Exit         as X

data Env r = Env
    { envSupportsAnsi :: !Bool
    , envStartClock   :: !TimeSpec
    , envR            :: r
    }

newtype Peu r a = Peu { unPeu :: Env r -> IO a }
  deriving stock Functor

runPeu :: forall a r. r -> Peu r a -> IO a
runPeu r m = withConcurrentOutput $ do
    supportsAnsi <- ANSI.hSupportsANSI stderr
    now <- getTime Monotonic

    let env :: Env r
        env = Env
            { envSupportsAnsi = supportsAnsi
            , envStartClock   = now
            , envR            = r
            }

    unPeu m env

changePeu :: (r -> s) -> Peu s a -> Peu r a
changePeu f (Peu m) = Peu $ \e -> m $ e { envR = f (envR e) }

instance Applicative (Peu r) where
    pure = \x -> Peu (\_ -> pure x)
    (<*>) = ap
    {-# INLINE (<*>) #-}

instance Monad (Peu r) where
    return = pure

    m >>= k = Peu $ \e -> unPeu m e >>= \x -> unPeu (k x) e
    {-# INLINE (>>=) #-}

instance MonadIO (Peu r) where
    liftIO m = Peu $ \_ -> m
    {-# INLINE liftIO #-}

instance MonadUnliftIO (Peu r) where
    withRunInIO f = Peu $ \r -> f $ \m -> unPeu m r

instance MonadReader r (Peu r) where
    ask      = Peu $ \e -> return (envR e)
    reader f = Peu $ \e -> return (f (envR e))

    local f (Peu g) = Peu $ \e -> g (e { envR = f (envR e) })

instance MonadThrow (Peu r) where
    throwM e = Peu $ \_ -> throwM e

instance MonadCatch (Peu r) where
    catch (Peu m) c = Peu $ \r -> m r `catch` \e -> unPeu (c e) r

instance MonadMask (Peu r) where
    mask a = Peu $ \e -> mask $ \u -> unPeu (a $ q u) e
      where
        q :: (IO a -> IO a) -> Peu r a -> Peu r a
        q u (Peu b) = Peu (u . b)

    uninterruptibleMask a =
        Peu $ \e -> uninterruptibleMask $ \u -> unPeu (a $ q u) e
      where
        q :: (IO a -> IO a) -> Peu r a -> Peu r a
        q u (Peu b) = Peu (u . b)

    generalBracket acquire release use = Peu $ \r -> generalBracket
        (unPeu acquire r)
        (\resource exitCase -> unPeu (release resource exitCase) r)
        (\resource -> unPeu (use resource) r)

-- instance LiftRegion

-------------------------------------------------------------------------------
-- Output
-------------------------------------------------------------------------------

withSetSgrCode :: (([ANSI.SGR] -> String) -> Peu r a) -> Peu r a
withSetSgrCode f = Peu $ \env -> unPeu (f (setSGRCode env)) env where
    setSGRCode env
        | envSupportsAnsi env = ANSI.setSGRCode
        | otherwise           = const ""

output :: String -> Peu r ()
output = liftIO . outputConcurrent . (++ "\n")

-------------------------------------------------------------------------------
-- Configuration
-------------------------------------------------------------------------------

class Warning w where
    warningToFlag :: w -> String

-------------------------------------------------------------------------------
-- Implementation details
-------------------------------------------------------------------------------

withTimeAndSetSgrCode :: (String -> ([ANSI.SGR] -> String) -> IO a) -> Peu r a
withTimeAndSetSgrCode f = Peu $ \env -> do
    now <- getTime Monotonic
    let TimeSpec s ns = diffTimeSpec now (envStartClock env)
    let off = printf "[%10.5f] " (fromIntegral s + fromIntegral ns / 1e9 :: Double)
    f off $
        if envSupportsAnsi env
        then ANSI.setSGRCode
        else const ""

-------------------------------------------------------------------------------
-- Diagnostics
-------------------------------------------------------------------------------

putDebug :: String -> Peu r ()
putDebug msg = withTimeAndSetSgrCode $ \t setSgr -> do
    let sgr :: [ANSI.SGR]
        sgr =
            [ ANSI.SetConsoleIntensity ANSI.BoldIntensity
            , ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Blue
            ]
    errorConcurrent $ concat
        [ t
        , setSgr sgr
        , "debug: "
        , setSgr []
        , msg
        , "\n"
        ]

putInfo :: String -> Peu r ()
putInfo msg = withTimeAndSetSgrCode $ \t setSgr -> do
    let sgr :: [ANSI.SGR]
        sgr =
            [ ANSI.SetConsoleIntensity ANSI.BoldIntensity
            , ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green
            ]
    errorConcurrent $ concat
        [ t
        , setSgr sgr
        , "info: "
        , setSgr []
        , msg
        , "\n"
        ]

putWarning :: Warning w => w -> String -> Peu r ()
putWarning  w msg = withTimeAndSetSgrCode $ \t setSgr -> do
    let sgr :: [ANSI.SGR]
        sgr =
            [ ANSI.SetConsoleIntensity ANSI.BoldIntensity
            , ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Magenta
            ]
    errorConcurrent $ concat
        [ t
        , setSgr sgr
        , "warning"
        , setSgr []
        , "["
        , setSgr sgr
        , "-W"
        , warningToFlag w
        , setSgr []
        , "]: "
        , msg
        , "\n"
        ]

putError :: String -> Peu r ()
putError msg = withTimeAndSetSgrCode $ \t setSgr -> do
    let sgr :: [ANSI.SGR]
        sgr =
            [ ANSI.SetConsoleIntensity ANSI.BoldIntensity
            , ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Red
            ]
    errorConcurrent $ concat
        [ t
        , setSgr sgr
        , "error:"
        , setSgr []
        , msg
        , "\n"
        ]

-------------------------------------------------------------------------------
-- Control.Exception
-------------------------------------------------------------------------------

evaluate :: NFData a => a -> Peu r a
evaluate = liftIO . X.evaluate

evaluateForce :: NFData a => a -> Peu r a
evaluateForce = evaluate . force

-------------------------------------------------------------------------------
-- System.Environment
-------------------------------------------------------------------------------

getArgs :: Peu r [String]
getArgs = liftIO X.getArgs

lookupEnv :: String -> Peu r (Maybe String)
lookupEnv = liftIO . X.lookupEnv

-------------------------------------------------------------------------------
-- System.Exit
-------------------------------------------------------------------------------

exitFailure :: Peu r a
exitFailure = liftIO X.exitFailure
