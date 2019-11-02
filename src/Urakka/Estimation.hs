module Urakka.Estimation (
    Estimator,
    mkEstimator,
    currentEstimate,
    addEstimationPoint,
    MonadSTM (..),
    ) where

import Prelude

import Control.Concurrent.STM
       (STM, newTVar, TVar, atomically, modifyTVar', readTVar, writeTVar)
import System.Clock             (TimeSpec, Clock (Monotonic), getTime, toNanoSecs)
import Urakka (ConcSt, urakkaOverEstimate, urakkaDone)
import Math.Regression.Simple   (V2 (..), linear, zerosLin)

-------------------------------------------------------------------------------
-- MonadSTM
-------------------------------------------------------------------------------

class Monad m => MonadSTM m where
    liftSTM :: STM a -> m a

instance MonadSTM IO where
    liftSTM = atomically

instance MonadSTM STM where
    liftSTM = id

-------------------------------------------------------------------------------
-- Estimator
-------------------------------------------------------------------------------

data Estimator = E
    { eStartTime :: TimeSpec
    , ePoints    :: TVar [Point]
    , eSmooth    :: TVar Smooth
    , eEstimate  :: TVar (Maybe Double)
    }

-- | Smoothing state
data Smooth
    = S0                      -- ^ initial state
    | S1 Double Double        -- ^ t0 x0
    | S Double Double Double  -- ^ t1 s1 b1

data Point = P
    { pTime  :: !Double
    , pDone  :: !Int
    , pTotal :: !Int
    }
  deriving Show

mkEstimator :: IO Estimator
mkEstimator = do
    startTime <- getTime Monotonic
    liftSTM $ do
        points   <- newTVar []
        smooth   <- newTVar S0
        estimate <- newTVar Nothing
        return (E startTime points smooth estimate)

currentEstimate :: MonadSTM m => Estimator -> m (Maybe Double)
currentEstimate e = liftSTM (readTVar (eEstimate e))

addEstimationPoint :: Estimator -> ConcSt -> IO ()
addEstimationPoint e concSt = do
    currTime  <- getTime Monotonic
    atomically $ do
        done     <- urakkaDone concSt
        total    <- urakkaOverEstimate concSt
        let point :: Point
            point = P ((* 1e-9) $ realToFrac $ toNanoSecs $ currTime - (eStartTime e)) done total
        modifyTVar' (ePoints e) (point :)
        estimate <- estimateDuration (eSmooth e) (ePoints e)
        writeTVar (eEstimate e) estimate

-------------------------------------------------------------------------------
-- Duration calculation
-------------------------------------------------------------------------------

estimateDuration :: TVar Smooth -> TVar [Point] -> STM (Maybe Double)
estimateDuration smoothVar statsVar = do
    st    <- readTVar smoothVar
    stats <- readTVar statsVar

    case stats of
        (P t _ limit' : _) | length stats > 10 -> do
            let limit :: Double
                limit = realToFrac limit'

                x = max t $ case linear [ (pTime sp, realToFrac (pDone sp) / limit) | sp <- take 10 stats ] of
                    V2 u v -> zerosLin (V2 u (v - 1))

            case st of
                  S0 -> do
                      writeTVar smoothVar (S1 t x)
                      return $ Just x

                  S1 t0 x0 -> do
                      writeTVar smoothVar (S t x $ (x - x0) / (t - t0))
                      return $ Just x

                  S t' s' b' -> do
                      -- parameters
                      let alpha = if x > s' then 0.9 else 0.2
                          beta  = 0.2

                      -- time scale
                      let scale = 10

                      -- time delta
                      let deltaT = (t - t') / scale

                      -- time adjusted parameters
                      let exp1alpha = (1 - alpha) ** deltaT
                          exp1beta  = (1 - beta)  ** deltaT

                      -- smoothed x
                      let s = (1 - exp1alpha) * x + exp1alpha * (s' + b' * deltaT)

                      -- smoothed argument delta
                      let deltaS = (s - s') / deltaT

                      -- trend
                      let b = (1 - exp1beta) * deltaS + exp1beta * b'

                      -- write state
                      writeTVar smoothVar (S t s b)

                      -- Estimate is smoothed s
                      return (Just s)

        _ -> return Nothing
