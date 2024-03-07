module StringToExpressionClass(parser) where
    import ArimeticExpression
    import Data.Maybe (fromMaybe)

    parser :: String -> Maybe Expression
    parser [] = Nothing
    parser string
        | dropUntilChar string '+' /= [] = buildExpression (returnUntilChar string '+') (dropUntilChar string '+')
        | dropUntilChar string '-' /= [] = buildExpression (returnUntilChar string '-') (dropUntilChar string '-')
        | dropUntilChar string '/' /= [] = buildExpression (returnUntilChar string '/') (dropUntilChar string '/')
        | dropUntilChar string '*' /= [] = buildExpression (returnUntilChar string '*') (dropUntilChar string '*')
        | dropUntilChar string '^' /= [] = buildExpression (returnUntilChar string '^') (dropUntilChar string '^')
        | otherwise = Just (Val (read string))


    buildExpression :: String -> String -> Maybe Expression
    buildExpression _ [] = Nothing
    buildExpression [] _ = Nothing
    buildExpression text (x:xs)
        | x == '+' = Just (Sum (parser text) (parser xs))
        | x == '-' = Just (Sub (parser text) (parser xs))
        | x == '*' = Just (Mul (parser text) (parser xs))
        | x == '^' = Just (Pow (parser text) (parser xs))
        | x == '/' = Just (Div (parser text) (parser xs))



    returnUntilChar :: String -> Char-> String
    returnUntilChar [] _ = []
    returnUntilChar string c = takeWhile (/= c) string

    dropUntilChar :: String -> Char-> String
    dropUntilChar [] _ = []
    dropUntilChar string c = dropWhile (/= c) string


