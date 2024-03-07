module Lib(someFunc) where

import ArimeticExpression
import ExpresionEvaluator
import StringToExpressionClass

someFunc :: IO ()
someFunc = print(evalExpression(parser "2+2*3") )


