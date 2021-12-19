module Trustee (main) where

import Trustee.Bounds
import Trustee.Sweep
import Trustee.Config
import Trustee.Get
import Trustee.Matrix
import Trustee.Monad
import Trustee.Options  (Cmd (..), goPlanParams, parseOpts, goTracer)

import Peura

main :: IO ()
main = do
    (opts, cmd) <- parseOpts
    tracer <- makeTracerPeu $ goTracer opts defaultTracerOptions
        { tracerOptionsProcess = False
        }
    let pp = goPlanParams opts
    runPeu tracer () $ readConfig >>= \cfg -> case cmd of
        CmdGet pkgname vr  -> cmdGet opts pkgname vr
        CmdBounds verify l -> runM cfg pp $ do
            cwd <- getCurrentDirectory
            runUrakkaM $ cmdBounds (hoistTracer (changePeu (const ())) tracer) opts cwd verify l
        CmdSweep verify -> runM cfg pp $ do
            cwd <- getCurrentDirectory
            runUrakkaM $ cmdBoundsSweep (hoistTracer (changePeu (const ())) tracer) opts cwd verify 
        CmdMatrix test dirs -> runM cfg pp $ do
            dirs' <- traverse makeAbsolute dirs
            runUrakkaM $ cmdMatrix opts test dirs'
