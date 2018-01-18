module Trustee.Matrix (cmdMatrix) where

import Data.Char                 (isDigit)
import Data.Function             (on)
import Data.Functor.Classes      (liftCompare)
import Data.List                 (sortBy)
import Data.Maybe                (mapMaybe)
import Data.Semigroup            ((<>))
import Distribution.Compat.ReadP (many, munch1, (+++))
import Distribution.Text         (display)
import Path                      (Abs, Dir, Path)

import qualified Path

import Trustee.GHC
import Trustee.Monad
import Trustee.NewBuild (Result (..), matrixRow')
import Trustee.Options
import Trustee.Table
import Trustee.Txt
import Trustee.Util

cmdMatrix :: GlobalOpts -> Bool -> [Path Abs Dir] -> M ()
cmdMatrix opts test dirs' = do
    let ghcs = reverse $ ghcsInRange (goGhcVersions opts)
    xss <- forConcurrently dirs $ matrixRow' test ghcs mempty

    putStrs [ renderTable $ makeTable ghcs xss ]

    -- TODO: some errors
    putStrs $ take 2 $ mapMaybe isFail $ concat xss
  where
    dirs = sortBy (liftCompare cmp `on` parts) dirs'
    parts
        = maybeReadP partsP
        . Path.toFilePath . Path.dirname

    partsP = many (fmap Left (munch1 (not . isDigit)) +++ fmap (Right . read) (munch1 isDigit))

    cmp :: [Either String Int] -> [Either String Int] -> Ordering
    cmp [] [] = EQ
    cmp [] _  = LT
    cmp _  [] = GT
    cmp (Right n : xs) (Right m : ys) = compare m n <> cmp xs ys
    cmp (Right _ : _)  (Left _  : _)  = LT
    cmp (_       : _)  (Right _ : _)  = GT
    cmp (Left s  : xs) (Left t  : ys) = compare s t <> cmp xs ys

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
    mkCell ResultTestFail   = mkTxt Yellow  "TEST"
    mkCell ResultDryFail {} = mkTxt Blue    "NO-IP"
    mkCell ResultDepFail {} = mkTxt Magenta "DEP"
    mkCell ResultFail {}    = mkTxt Red     "FAIL"

    isFail (ResultFail _ o e) = Just (o ++ "\n" ++ e)
    isFail _                  = Nothing
