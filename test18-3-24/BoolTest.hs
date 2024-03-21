module BoolTest where
    data Expr = Var Char
        | Not (Maybe Expr)
        | And (Maybe Expr) (Maybe Expr)
        | Or (Maybe Expr) (Maybe Expr)
        deriving (Show)

    parserExpr :: String -> Maybe Expr
    parserExpr [] = Nothing
    parserExpr [x] = Just (Var x)
    parserExpr str  | head str == '(' && takeuntilsimbol str ')' /= [] && dropuntilsimbol str ')' /= [] 
                        && tail (dropuntilsimbol str ')') /= [] && dropuntilsimbol (tail (takeuntilsimbol str ')')) '(' == []
                        = parserExprVal (takeuntilsimbol (tail str) ')') (tail (dropuntilsimbol str ')'))
                    | head str == '(' && takeuntilsimbol str ')' /= [] && last str /= ')' 
                        = parserExprVal (init(takeuntillastsimbol (tail str) ')')) (tail (dropuntillastsimbol str ')'))
                    | head str == '(' && takeuntilsimbol str ')' /= [] = parserExpr (init (tail str))
                    | dropuntilsimbol str '(' /= [] && takeuntilsimbol str '(' /= [] = parserExprVal (takeuntilsimbol str '(') (dropuntilsimbol str '(')
                    | dropuntilsimbol str '(' /= []  = parserExpr (dropuntilsimbol str '(')
                    | dropuntilsimbol str 'v' /= []  =  parserExprVal (takeuntilsimbol str 'v') (dropuntilsimbol str 'v')
                    | dropuntilsimbol str '^' /= []  = parserExprVal (takeuntilsimbol str '^') (dropuntilsimbol str '^')
                    | dropuntilsimbol str '~' /= []  = parserExprVal (takeuntilsimbol str '~') (dropuntilsimbol str '~')
                    | otherwise = Nothing


    parserExprVal :: String -> String -> Maybe Expr
    parserExprVal _ [] = Nothing
    parserExprVal [] (x:xs) | x == '~' = Just (Not (parserExpr xs))
                            | otherwise = Nothing
    parserExprVal str (x:xs)    | x == 'v' = Just (Or (parserExpr str) (parserExpr xs))
                                | x == '^' = Just (And (parserExpr str) (parserExpr xs))
                                | otherwise = parserExprVal (init str) (last str : x:xs)

    takeuntilsimbol :: String -> Char -> String
    takeuntilsimbol [] _ = []
    takeuntilsimbol str c = takeWhile (/= c) str

    takeuntillastsimbol :: String -> Char -> String
    takeuntillastsimbol [] _ = []
    takeuntillastsimbol str c   | dropuntilsimbol str c /= [] = takeWhile (/= c) str ++ [c] ++ takeuntillastsimbol (dropWhile (== c) (dropWhile (/= c) str)) c
                                | otherwise = []

    dropuntilsimbol :: String -> Char -> String
    dropuntilsimbol [] _ = []
    dropuntilsimbol str c = dropWhile (/= c) str

    dropuntillastsimbol :: String -> Char -> String
    dropuntillastsimbol [] _ = []
    dropuntillastsimbol str c   | takeWhile (/= c) str /= str = dropuntillastsimbol (tail (dropuntilsimbol str c)) c
                                | otherwise = c:str

    solver :: String -> Maybe Bool
    solver [] = Nothing
    solver [x]  | x == '^' = Nothing
                | x == 'v' = Nothing
                | x == '~' = Nothing
                | otherwise = Just (solveExpr (parserExpr (show x)))
    solver str = Just (solveExpr (parserExpr str))


    solveExpr :: Maybe Expr -> Bool
    solveExpr Nothing = False
    solveExpr (Just (Or ex1 ex2)) = solveExpr ex1 || solveExpr ex2
    solveExpr (Just (And ex1 ex2)) = solveExpr ex1 && solveExpr ex2
    solveExpr (Just (Not ex1)) = not (solveExpr ex1)
    solveExpr (Just (Var ex1))      | ex1 == 'a' = True
                                    | ex1 == 'b' = True
                                    | ex1 == 'c' = False
                                    
    

