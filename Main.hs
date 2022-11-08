import Sudoku
import Display
import Data.List

f :: Int -> Cell
f = FixedCell

e :: Cell
e = EmptyCell

puzzle :: Sudoku
puzzle = Sudoku [[f 1, f 5, e  , e  , f 4, f 2, e  , e  , f 6],
                 [f 2, f 7, f 4, f 5, f 6, e  , e  , f 1, e  ],
                 [e  , e  , f 6, e  , e  , f 7, f 4, e  , f 2],
                 [e  , f 1, e  , e  , e  , e  , e  , f 4, e  ],
                 [e  , e  , e  , e  , f 5, e  , e  , e  , e  ],
                 [e  , f 6, e  , f 4, e  , f 3, f 1, f 9, e  ],
                 [e  , f 2, e  , f 6, e  , f 5, f 9, e  , e  ],
                 [f 9, f 8, f 5, e  , f 3, e  , e  , f 6, e  ],
                 [e  , f 4, e  , f 2, f 1, f 9, f 8, f 3, e  ]
                ]

main = do
  let sols = getSolutions puzzle
  putStrLn $ prettyPrint $ head sols
  
