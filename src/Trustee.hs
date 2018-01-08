module Trustee (main) where

import Path.IO (getCurrentDir)

import Trustee.Bounds
import Trustee.Config
import Trustee.Get
import Trustee.Matrix
import Trustee.Monad
import Trustee.NewBuild
import Trustee.Options  (Cmd (..), parseOpts)

import qualified Path.IO as Path

main :: IO ()
main = do
    cwd <- getCurrentDir
    (opts, cmd) <- parseOpts
    let cfg = defaultConfig
    case cmd of
        CmdBounds verify l -> runM cfg $ cmdBounds opts cwd verify l
        CmdNewBuild args   -> runM cfg $ cmdNewBuild opts cwd args
        CmdGet pkgname vr  -> cmdGet pkgname vr
        CmdMatrix dirs cs  -> do
            dirs' <- traverse (Path.resolveDir cwd) dirs
            runM cfg $ cmdMatrix opts dirs' cs
