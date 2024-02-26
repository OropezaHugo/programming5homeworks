
module Parenthesizedfunction where
    parenthesizedfunction :: String -> [String]
    parenthesizedfunction [] = []
    parenthesizedfunction str = split str ' '
    

    split :: String -> Char -> [String]
    split [] _ = []
    split str c = split2 "" str c


    split2 :: String -> String -> Char -> [String]
    split2 str (x:xs) c 
        | x == c && xs == [] && str /= [] = [str ++ ")"]
        | x == c && str /= [] = [str ++ ")"] ++ split2 "(" xs c
        | x == c = split2 [] xs c
        | xs == [] && str == [] = [[x]]
        | xs == [] = [str ++ [x] ++ ")"]
        | str == [] = split2 ("(" ++ str ++ [x]) xs c 
        | otherwise = split2 (str ++ [x]) xs c