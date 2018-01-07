module Trustee (main) where

import Path.IO (getCurrentDir)

import Trustee.Config
import Trustee.LowerBounds
import Trustee.Monad
import Trustee.NewBuild
import Trustee.Get
import Trustee.Matrix
import Trustee.Options     (Cmd (..), parseOpts)

import qualified Path.IO as Path

main :: IO ()
main = do
    cwd <- getCurrentDir
    (opts, cmd) <- parseOpts
    let cfg = defaultConfig
    case cmd of
        CmdLowerBounds verify -> runM cfg $ cmdLowerBounds opts cwd verify
        CmdNewBuild args  -> runM cfg $ cmdNewBuild opts cwd args
        CmdGet pkgname vr -> cmdGet pkgname vr
        CmdMatrix dirs cs -> do
            dirs' <- traverse (Path.resolveDir cwd) dirs
            runM cfg $ cmdMatrix opts dirs' cs
