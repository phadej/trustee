{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
module VendingMachine (
    makeVendingMachine,
    withWishes,
    Wishes,
    ) where

import Prelude

import Control.Applicative.Free
import Control.Concurrent        (threadDelay)
import Control.Concurrent.Async  (async, cancel, mapConcurrently_, wait)
import Control.Concurrent.STM
import Control.Exception
import Control.Monad             (forever, unless, when)
import Data.Bifunctor            (first)
import Data.Char                 (ord)
import Data.Kind                 (Type)
import Data.List                 (sortOn)
import Data.List                 (foldl')
import Data.Monoid               (Sum (..))
import System.Console.Concurrent (outputConcurrent, withConcurrentOutput)

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

-------------------------------------------------------------------------------
-- Example
-------------------------------------------------------------------------------

data C a where
    CInt  :: Char -> C Int
    CBool :: C Bool

{-
Thread created'a'
Thread created'b'
Thread created'c'
Thread created'd'
Thread created'e'
Thread created'f'
Thread created'g'
Thread created'h'
Thread created'i'
Thread created'k'
Thread created'm'
Thread created'l'
Thread running('b',0,True)
Thread running('a',1,False)
Thread running('c',2,True)
Thread closing'b'
Thread running('d',2,False)
Thread closing'c'
Thread closing'a'
Thread running('e',2,True)
Thread running('f',2,False)
Thread closing'd'
Thread running('g',2,True)
Thread closing'f'
Thread closing'e'
Thread running('h',2,False)
Thread running('i',2,True)
Thread closing'g'
Thread running('k',2,False)
Thread closing'h'
Thread closing'i'
Thread running('l',2,True)
Thread running('m',2,False)
Thread closing'k'
Thread closing'l'
Thread closing'm'
-}

main :: IO ()
main = do
    ints  <- newTVarIO (0 :: Int)
    bools <- newTVarIO True

    let supply :: C a -> STM (a, STM ())
        supply (CInt _) = do
            i <- readTVar ints
            when (i > 2) retry -- semaphore!
            writeTVar ints (i + 1)
            return (i, modifyTVar ints pred)

        supply CBool = do
            b <- readTVar bools
            writeTVar bools (not b)
            return (b, return ())

    -- we price by name
    let price :: C a -> Sum Int
        price (CInt c) = Sum (ord c)
        price CBool    = mempty

    (vm, stop) <- makeVendingMachine supply price

    withConcurrentOutput $ mapConcurrently_ id
        [ do
            outputConcurrent $ "Thread created" ++ show name ++ "\n"
            threadDelay 1000
            withWishes vm ((,) <$> liftAp (CInt name) <*> liftAp CBool) $ \(i, b) -> do
                outputConcurrent $ "Thread running" ++ show (name, i, b) ++ "\n"
                threadDelay 100000
                outputConcurrent $ "Thread closing" ++ show name ++ "\n"
        | name <- "abcdefghiklm"
        ]

    stop

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
runVendingMachine vm@(VendingMachine orders supply _ close) = do
    done <- atomically $ do
        x <- tryReadTChan close
        case x of
            Just () -> return True
            Nothing -> do
                orders'' <- readTVar orders
                let orders' = sortOn (\(Order _ _ p) -> p) orders''
                foldl' orElse retry
                    [ do
                        a <- unW (runAp (W . supply) wishes)
                        putTMVar tmvar a
                        writeTVar orders orders''
                    | (Order wishes (Ticket tmvar) _, orders'') <- unconsAny orders'
                    ]
                return False

    unless done (runVendingMachine vm)

unconsAny :: [a] -> [(a, [a])]
unconsAny = go [] where
    go ls []     = []
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
