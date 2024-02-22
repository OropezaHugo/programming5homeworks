module Main (main) where

import Lib()
import Hanoi


main :: IO ()
main = do
    let result = myHanoi(4, 'a', 'b', 'c')
    print result
