module Main (main) where

import Lib
import System.Console.ANSI
import System.IO
import TicTacToe
import Prelude



main :: IO ()
--main = printBoard emptyBoard >> printBoard (updateBoard 'c' 1 1 (updateBoard 'c' 0 0 (updateBoard 'c' 2 2 emptyBoard)))
-- >> putStrLn (boolToString(reviewWin (updateBoard 'c' 1 1 (updateBoard 'c' 0 0 (updateBoard 'c' 2 2 emptyBoard)))))
main = startTicTacToe