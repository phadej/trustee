module Trustee (main) where

import Path.IO (getCurrentDir)

import Trustee.LowerBounds
import Trustee.Monad
import Trustee.NewBuild
import Trustee.Config
import Trustee.Options    (parseOpts, Cmd (..))

main :: IO ()
main = do
    cwd <- getCurrentDir
    (opts, cmd) <- parseOpts
    let cfg = defaultConfig
    runM cfg $ case cmd of
        CmdLowerBounds -> cmdLowerBounds opts cwd
        CmdNewBuild args -> cmdNewBuild opts cwd args
