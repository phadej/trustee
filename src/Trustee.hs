module Trustee (main) where

import Trustee.Bounds
import Trustee.Config
import Trustee.Get
import Trustee.Matrix
import Trustee.Monad
import Trustee.Options  (Cmd (..), goPlanParams, parseOpts)

import Peura

main :: IO ()
main = runPeu () $ configure >>= \(opts, pp, cmd, cfg) -> case cmd of
    CmdGet pkgname vr  -> cmdGet opts pkgname vr
    CmdBounds verify l -> runM cfg pp $ do
        cwd <- getCurrentDirectory
        runUrakkaM $ fmap ((,) (return "")) $ cmdBounds opts cwd verify l
    CmdMatrix test dirs -> runM cfg pp $ do
        dirs' <- traverse makeAbsolute dirs
        runUrakkaM $ cmdMatrix opts test dirs'
  where
    configure = do
        (opts, cmd) <- liftIO parseOpts
        let pp = goPlanParams opts
        cfg <- readConfig
        return (opts, pp, cmd,cfg)
      
