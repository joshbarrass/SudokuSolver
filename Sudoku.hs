module Sudoku
  (
  Sudoku(..)
  ,Cell(..)
  ,getSolutions
  ) where

data Cell = FixedCell {fixedValue :: Int} | EmptyCell deriving (Show, Eq)
 
-- Determine whether a given cell is a FixedCell or not
isFixed :: Cell -> Bool
isFixed (FixedCell _) = True
isFixed EmptyCell = False


data Sudoku = Sudoku { cells :: [[Cell]] } deriving (Show)

-- Get the row for a given (x,y) coordinate from a Sudoku, excluding that cell
getRow :: Sudoku -> (Int, Int) -> [Cell]
getRow sudoku (x, y) = take x row ++ drop (x+1) row
  where row = cells sudoku !! y

-- Get the column for a given (x,y) coordinate from a Sudoku, excluding that cell
getColumn :: Sudoku -> (Int, Int) -> [Cell]
getColumn sudoku (x, y) = [row !! x | (i, row) <- zip [0..] (cells sudoku), i /= y]

getGroup :: Sudoku -> (Int, Int) -> [Cell]
getGroup sudoku (x, y) = let
  gRows = map (\xs -> take 3 (drop gx xs)) (take 3 (drop gy $ cells sudoku))
  inRow = gRows !! in_y
  in concat $ (take in_y gRows) ++ (take in_x inRow ++ drop (in_x+1) inRow) : (drop (in_y+1) gRows)
  where gx = floor (fromIntegral x/3) * 3
        gy = floor (fromIntegral y/3) * 3
        in_x = x - gx
        in_y = y - gy

-- Get the cell at the given (x,y) coordinate from a Sudoku
getCell :: Sudoku -> (Int, Int) -> Cell
getCell sudoku (x, y) = row !! x
  where row = cells sudoku !! y

-- Given a list of cells, return a list of the fixed numbers
fixedNumbers :: [Cell] -> [Int]
fixedNumbers [] = []
fixedNumbers (FixedCell x:xs) = x : fixedNumbers xs
fixedNumbers (EmptyCell:xs) = fixedNumbers xs

-- Given a list of cells, return a list of the remaining numbers
remainingNumbers :: [Cell] -> [Int]
remainingNumbers xs = [n | n <- [1..9], n `notElem` fixedNumbers xs]

-- Determine the possibilities for a coordinate in a Sudoku
possibilities :: Sudoku -> (Int, Int) -> [Int]
possibilities sudoku coord
  | isFixed c = [fixedValue c]
  | otherwise = remainingNumbers $ getRow sudoku coord ++ getColumn sudoku coord ++ getGroup sudoku coord
  where c = getCell sudoku coord

-- Replace a given cell in a sudoku with a fixed cell
fixCell :: Sudoku -> (Int, Int) -> Int -> Sudoku
fixCell (Sudoku cells) (x, y) v = Sudoku (take y cells ++ (take x theRow ++ FixedCell v : drop (x+1) theRow) : drop (y+1) cells)
  where theRow = cells !! y

-- TODO: Algorithm for solving. Should be recursive and functions by
-- replacing empty cells with fixed cells corresponding to one of the
-- possible values they can hold. If you encounter any cell with no
-- remaining possibilities, the solution is invalid and you have to
-- wind back

-- Returns True if all cells in a flat list are fixed
allFixed :: [Cell] -> Bool
allFixed = all isFixed

-- Returns True if all cells in a Sudoku are fixed, but does not check
-- the validity of the solution
solved :: Sudoku -> Bool
solved = allFixed . concat . cells

-- Convert a Sudoku into a list of cells and their coordinates
coordList :: Sudoku -> [(Cell, (Int, Int))]
coordList sudoku = [(getCell sudoku (x, y), (x, y)) | x <- [0..(w-1)], y <- [0..(h-1)]]
  where h = length $ cells sudoku
        w = length $ head (cells sudoku)

-- Get the coordinate of the first non-fixed cell
getNonFixed :: Sudoku -> (Int, Int)
getNonFixed sudoku = snd $ head (dropWhile (isFixed . fst) cl)
  where cl = coordList sudoku

getSolutions :: Sudoku -> [Sudoku]
getSolutions sudoku
  | solved sudoku = [sudoku]
  | otherwise = let
      fnf = getNonFixed sudoku
      fnfPoss = possibilities sudoku fnf
      valid = fnfPoss /= []
      in if not valid then [] else
        let
          newSudokus = [fixCell sudoku fnf v | v <- fnfPoss]
        in concat [getSolutions s | s <- newSudokus]
