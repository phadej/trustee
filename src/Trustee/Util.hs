module Trustee.Util (
    -- * Early exit monad
    runEarlyExit,
    exit,
    lift,
    -- * ReadP
    maybeReadP,
    ) where

import Control.Monad.Trans.Class    (lift)
import Control.Monad.Trans.Except   (ExceptT (..), runExceptT)
import Data.Char                    (isSpace)
import Data.Maybe                   (listToMaybe)
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)

import Peura

runEarlyExit :: Functor m => ExceptT a m a -> m a
runEarlyExit = fmap (either id id) . runExceptT

exit :: Monad m => a -> ExceptT a m b
exit x = ExceptT (return (Left x))

maybeReadP :: ReadP a -> String -> Maybe a
maybeReadP p s = listToMaybe
    [ x
    | (x, rest) <- readP_to_S p s
    , all isSpace rest
    ]
