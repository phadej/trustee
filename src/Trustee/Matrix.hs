module Trustee.Matrix (cmdMatrix) where

import Data.Function             (on)
import Data.List                 (sortBy)
import Distribution.Compat.ReadP (munch1, satisfy, sepBy)
import Distribution.Text         (display)
import Path                      (Abs, Dir, Path)

import qualified Path

import Trustee.GHC
import Trustee.Monad
import Trustee.NewBuild (Result (..), matrixRow)
import Trustee.Options
import Trustee.Table
import Trustee.Txt
import Trustee.Util

cmdMatrix :: GlobalOpts -> [Path Abs Dir] -> [String] -> M ()
cmdMatrix opts dirs' cons = do
    let ghcs = reverse $ ghcsInRange (goGhcVersions opts)
    let consOpts = map ("--constraint=" ++) cons
    xss <- forConcurrently dirs $
        matrixRow ghcs $ ["--disable-tests", "--disable-benchmarks", "all" ] ++ consOpts

    putStrs [ renderTable $ makeTable ghcs xss ]
  where
    -- TODO: parse as package identifier and compare on that
    dirs = sortBy (flip compare `on` parts) dirs'
    parts
        = maybeReadP (sepBy (munch1 (`notElem` "._-/")) (satisfy (`elem`  "._-/")))
        . Path.toFilePath . Path.dirname

    makeTable :: [GHCVer] -> [[Result]] -> [[Txt]]
    makeTable ghcs xss
        = (emptyTxt : map (mkTxt Black . display. toVersion) ghcs)
        : zipWith mkRow dirs xss

    mkRow :: Path Abs Dir -> [Result] -> [Txt]
    mkRow p xs
        = mkTxt Black (Path.toFilePath $ Path.dirname p)
        : map mkCell xs

    mkCell :: Result -> Txt
    mkCell ResultOk         = mkTxt Green   "OK"
    mkCell ResultDryFail {} = mkTxt Blue    "NO-IP"
    mkCell ResultDepFail {} = mkTxt Magenta "DEP"
    mkCell ResultFail {}    = mkTxt Red     "FAIL"
