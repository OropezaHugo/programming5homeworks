module StringToExpression(stringToExpression) where
    import ArimeticExpression

    stringToExpression :: String -> Expression
    stringToExpression [] = Val 0
    stringToExpression [x] = (Val (read [x]))
    stringToExpression [x, y] = Val 0
    stringToExpression (x:xs) = stringToExpression123 xs (Val (read [x]))

    stringToExpression123 :: String -> Expression -> Expression
    stringToExpression123 [] _ = Val 0
    
    stringToExpression123 [x,y] ex1
        | x == '+' = Sum ex1 (Val (read [y]))
        | x == '*' = Mult ex1 (Val (read [y]))
        | x == '/' = Div ex1 (Val (read [y]))
        | x == '^' = Pow ex1 (Val (read [y]))
        | otherwise = Val (read [y])

    stringToExpression123 (x:xs) ex1
        | x == '+' = Sum ex1 (stringToExpression xs)
        | x == '*' = Mult ex1 (stringToExpression xs)
        | x == '/' = Div ex1 (stringToExpression xs)
        | x == '^' = Pow ex1 (stringToExpression xs)
        | otherwise = stringToExpression123 xs (Val (read [x]))
        
