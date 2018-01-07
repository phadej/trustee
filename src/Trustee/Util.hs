module Trustee.Util (
    runEarlyExit,
    exit,
    lift,
    ) where

import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Class  (lift)

runEarlyExit :: Functor m => ExceptT a m a -> m a
runEarlyExit = fmap (either id id) . runExceptT

exit :: Monad m => a -> ExceptT a m b
exit x = ExceptT (return (Left x))
