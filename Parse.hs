module Parse
  (
    parseSudoku
  ) where 

import Sudoku
import Data.Char

parseSudokuLine :: String -> [Cell]
parseSudokuLine [] = []
parseSudokuLine ('.':xs) = EmptyCell : parseSudokuLine xs
parseSudokuLine (x:xs)
  | isDigit x = FixedCell (read [x]) : parseSudokuLine xs
  | otherwise = parseSudokuLine xs

-- Parses a Sudoku written out in text format. Only numbers, full
-- stops and line breaks will be parsed. Full stops represent empty
-- cells. Other characters will be ignored to allow structuring the
-- text however you please.
parseSudoku :: String -> Sudoku
parseSudoku s = let
  rows = lines s
  cells = filter ( /= []) $ map parseSudokuLine rows
  in Sudoku cells
