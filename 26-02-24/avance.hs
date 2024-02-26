module Parenthesizedfunction where
    parenthesizedfunction :: String -> [String]
    parenthesizedfunction str = []
    

    split :: String -> Char -> [String]
    split [] _ = []
    split (x:xs) c | x == c = split xs c
        | otherwise = [[x]] ++ split xs c


    split2 :: String -> String -> Char -> [String]
    split2 [] [] _ = []
    split2 str (x:xs) c | x == c = [str] ++ split2 [] xs c
        | xs == [] = [str] ++ [[x]]
        | otherwise = split2 (str ++ [x]) xs c