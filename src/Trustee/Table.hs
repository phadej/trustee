module Trustee.Table (renderTable) where

import Data.List (intercalate)

import Trustee.Txt

import Peura

-- | Render list of rows
renderTable :: [[Txt]] -> String
renderTable xss = unlines $ map row xss
  where
    ws = foldl merge [] xss

    row xs = intercalate "  " $ map txtToString $ zipWith txtLeftPad xs ws

merge :: [Int] -> [Txt] -> [Int]
merge []       []       = []
merge []       xs       = map txtLength xs
merge ls       []       = ls
merge (l : ls) (x : xs) = max l (txtLength x) : merge ls xs
