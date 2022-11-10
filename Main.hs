import Sudoku
import Display
import Parse

import System.Environment
import System.Directory

printSol :: Sudoku -> IO ()
printSol puzzle = do
  let sols = getSolutions puzzle
  putStrLn $ prettyPrint $ head sols

main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  fileExists <- doesFileExist filename
  if fileExists then do
    puzzleStr <- readFile (head args)
    let puzzle = parseSudoku puzzleStr
    printSol puzzle
  else do
    putStrLn "Warning: file does not exist; interpreting arg as a puzzle string\n"
    let puzzle = parseSudoku filename
    printSol puzzle
  
