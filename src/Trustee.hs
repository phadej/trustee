module Trustee (main) where

import Trustee.LowerBounds
import Trustee.Monad
import Trustee.NewBuild
import Trustee.Config
import Trustee.Options    (parseOpts, Cmd (..))

main :: IO ()
main = do
    (opts, cmd) <- parseOpts
    let cfg = defaultConfig
    runM cfg $ case cmd of
        CmdLowerBounds -> cmdLowerBounds opts (error "path")
        CmdNewBuild args -> cmdNewBuild opts args
