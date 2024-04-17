module Main (main) where

import Lib
import Data.Char
import Data.List
import System.IO
import Game(play, run, empty, Player(..))

main :: IO ()
main = do 
            hSetBuffering stdout NoBuffering
            run empty O
            --play empty O
