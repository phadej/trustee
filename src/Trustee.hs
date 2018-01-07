module Trustee (main) where

import Path.IO (getCurrentDir)

import Data.Functor        (void)
import Trustee.Config
import Trustee.LowerBounds
import Trustee.Monad
import Trustee.NewBuild
import Trustee.Options     (Cmd (..), parseOpts)

import qualified Path.IO as Path

main :: IO ()
main = do
    cwd <- getCurrentDir
    (opts, cmd) <- parseOpts
    let cfg = defaultConfig
    case cmd of
        CmdLowerBounds   -> runM cfg $ cmdLowerBounds opts cwd
        CmdNewBuild args -> runM cfg $ cmdNewBuild opts cwd args
        CmdMatrix dirs   -> do
            dirs' <- traverse (Path.resolveDir cwd) dirs
            runM cfg $ void $ forConcurrently dirs' $ \d ->
                cmdNewBuild opts d ["--disable-tests", "--disable-benchmarks", "all" ]
