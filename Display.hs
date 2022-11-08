module Display (
  prettyPrint
  ) where

import Sudoku
import Data.List

insertEvery :: [a] -> Int -> [a] -> [a]
insertEvery x 0 ys = ys
insertEvery x n [] = []
insertEvery x n ys
  | length ys < n = ys
  | otherwise = take n ys ++ x ++ insertEvery x n (drop n ys)

prettyPrint :: Sudoku -> String
prettyPrint (Sudoku cells) = let
  stringCells = [map (show . fixedValue) r | r <- cells]
  columnBars = [init $ insertEvery "|" 3 (concat r) | r <- stringCells]
  spacedRows = map ((' ' :) . init . insertEvery " " 1) columnBars
  rowBars = init $ insertEvery ["-------+-------+-------"] 3 spacedRows
  in intercalate "\n" rowBars
