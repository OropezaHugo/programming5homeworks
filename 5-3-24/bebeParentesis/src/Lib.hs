module Lib(someFunc) where

import ArimeticExpression
import ExpresionEvaluator
import StringToExpression

someFunc :: IO ()
someFunc = print(stringToExpression "2+2*5")


