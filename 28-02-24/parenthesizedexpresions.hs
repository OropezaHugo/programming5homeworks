module Parenthesizedexpresions where

    parenthesizesexpression :: String -> String
    parenthesizesexpression "" = ""
    parenthesizesexpression str = addParenthesizes "" str

    addParenthesizes :: String -> String -> String
    addParenthesizes str [x] = str ++ [x]
    addParenthesizes str [x,y] = addParenthesizes (str ++ [x]) ([y])
    addParenthesizes str [x,y,z] = addParenthesizes (str ++ [x]) ([y])
    addParenthesizes str str2 = addparentesizestooperator  "" (addparentesizestooperator str str2 '^') '*'

    addparentesizestooperator :: String -> String -> Char -> String
    addparentesizestooperator str [x] _ = str ++ [x]
    addparentesizestooperator str [x,y] c = addparentesizestooperator (str ++ [x]) ([y]) c
    addparentesizestooperator str (x:y:z:xs) c
        | y == c && x == ')' && z == '(' = addparentesizestooperator (reverse (addopenparenthesize "" (reverse(str ++ [x] ++ [y])))) (addcloseparenthesize "" ([z] ++xs)) c
        | y == c && x == ')' = addparentesizestooperator (reverse (addopenparenthesize "" (reverse(str ++ [x] ++ [y] ++ [z])))) (')':xs) c
        | y == c && z == '(' = str ++ ['('] ++ [x] ++ [y] ++ [z] ++ addcloseparenthesize "" xs
        | y == c = addparentesizestooperator (str ++ ['('] ++ [x] ++ [y] ++ [z]) (')':xs) c
        | otherwise = addparentesizestooperator (str ++ [x]) (y:z:xs) c

    addcloseparenthesize:: String -> String -> String
    addcloseparenthesize str (x:xs) 
        | x == ')' = str ++ [x] ++ [')'] ++ xs
        | otherwise = addcloseparenthesize (str ++ [x]) xs

    addopenparenthesize :: String -> String -> String
    addopenparenthesize str (x:xs) 
        | x == '(' = str ++ ['('] ++ [x] ++ xs
        | otherwise = addopenparenthesize (str ++ [x]) xs

    