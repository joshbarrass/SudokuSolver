import Sudoku
import Display
import Parse

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  puzzleStr <- readFile (head args)
  let puzzle = parseSudoku puzzleStr
  let sols = getSolutions puzzle
  putStrLn $ prettyPrint $ head sols
  
