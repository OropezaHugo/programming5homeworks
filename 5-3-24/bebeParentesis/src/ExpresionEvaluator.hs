module ExpresionEvaluator (evalExpression) where

import ArimeticExpression
import StringToExpressionClass
import Data.Maybe (fromMaybe)

evalExpression :: Maybe Expression -> Int
evalExpression (Just (Val n)) = n
evalExpression (Just (Sum x y)) = evalExpression x + evalExpression y
evalExpression (Just (Mul x y)) = evalExpression x * evalExpression y
evalExpression (Just (Pow x y)) = evalExpression x ^ evalExpression y
evalExpression (Just (Sub x y)) = evalExpression x - evalExpression y
evalExpression (Just (Div x y)) = evalExpression x `div` evalExpression y

