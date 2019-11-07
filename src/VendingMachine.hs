{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
module VendingMachine (
    VendingMachine,
    makeVendingMachine,
    withWishes,
    Wishes,
    wish,
    ) where

import Prelude

import Control.Applicative.Free
import Control.Concurrent.Async  (async, cancel)
import Control.Concurrent.STM
import Control.Exception
import Control.Monad             (unless, when)
import Data.Bifunctor            (first)
import Data.Kind                 (Type)
import Data.List                 (foldl', sortOn)

-- | Make a vending machine.
makeVendingMachine
    :: (Ord p, Monoid p)
    => (forall a. f a -> STM (a, STM ()))  -- ^ candy supply
    -> (forall a. f a -> p)                -- ^ candy prices
    -> IO (VendingMachine f, IO ())        -- ^ vending machine, and action to stop it.
makeVendingMachine supply price = do
    orders <- newTVarIO []
    close  <- newTChanIO
    let vm = VendingMachine orders supply price close
    a <- async $ runVendingMachine vm
    return (vm, do
        atomically $ writeTChan close ()
        cancel a)

withWishes :: VendingMachine f -> Wishes f a -> (a -> IO r) -> IO r
withWishes vm order kont = do
    Ticket open <- atomically $ placeOrder vm order
    bracket (atomically (takeTMVar open)) (atomically . snd) (kont . fst)

wish :: f a -> Wishes f a
wish = liftAp

-------------------------------------------------------------------------------
-- Private
-------------------------------------------------------------------------------

data Order f p where
    Order :: Ap f a -> Ticket a -> p -> Order f p

data VendingMachine (f :: Type -> Type) where
    VendingMachine
        :: (Monoid p, Ord p)
        => TVar [Order f p]
        -> (forall a. f a -> STM (a, STM ()))
        -> (forall a. f a -> p)
        -> TChan ()
        -> VendingMachine f

type Wishes = Ap

newtype Ticket a = Ticket (TMVar (a, STM ()))

placeOrder :: VendingMachine f -> Wishes f a -> STM (Ticket a)
placeOrder (VendingMachine orders _ price _) wishes = do
    open <- newEmptyTMVar
    let ticket = Ticket open
    let totalPrice = runAp_ price wishes
    modifyTVar' orders (Order wishes ticket totalPrice :)
    return ticket

runVendingMachine
    :: VendingMachine f
    -> IO ()
runVendingMachine vm@(VendingMachine ordersVar supply _ close) = do
    done <- atomically $ do
        x <- tryReadTChan close
        case x of
            Just () -> return True
            Nothing -> do
                orders0 <- readTVar ordersVar
                when (null orders0) retry
                let orders1 = sortOn (\(Order _ _ p) -> p) orders0
                foldl' orElse retry
                    [ do
                        a <- unW (runAp (W . supply) wishes)
                        putTMVar tmvar a
                        writeTVar ordersVar orders2
                    | (Order wishes (Ticket tmvar) _, orders2) <- unconsAny orders1
                    ]
                return False

    unless done (runVendingMachine vm)

unconsAny :: [a] -> [(a, [a])]
unconsAny = go [] where
    go _  []     = []
    go ls (r:rs) = (r, ls ++ rs) :  go (ls ++ [r]) rs

-------------------------------------------------------------------------------
-- Helper
-------------------------------------------------------------------------------

-- | Compose (Writer (STM ()) STM
newtype W a = W { unW :: STM (a, STM ()) }

instance Functor W where
    fmap f (W x) = W (fmap (first f) x)

instance Applicative W where
    pure x = W (pure (x, pure ()))
    W f <*> W x = W $ do
        (fo, fc) <- f
        (xo, xc) <- x
        return (fo xo, fc >> xc)
