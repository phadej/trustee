module Trustee (main) where

import Trustee.Bounds
import Trustee.Config
import Trustee.Get
import Trustee.Matrix
import Trustee.Monad
import Trustee.NewBuild
import Trustee.Options  (Cmd (..), goPlanParams, parseOpts)

import qualified System.Path.IO as Path

main :: IO ()
main = do
    cwd <- Path.getCurrentDirectory
    (opts, cmd) <- parseOpts
    let pp = goPlanParams opts
    cfg <- readConfig
    case cmd of
        CmdBounds verify l -> runM cfg pp $ cmdBounds opts cwd verify l
        CmdNewBuild args   -> runM cfg pp $ cmdNewBuild opts cwd args
        CmdGet pkgname vr  -> cmdGet opts pkgname vr
        CmdMatrix test dirs -> do
            dirs' <- traverse Path.makeAbsolute dirs
            runM cfg pp $ cmdMatrix opts test dirs'
