module Parenthesizedinclass where
    split :: String -> Char -> [String]
    split [] _ = []
    split (x:xs) c | x == c = split xs c
        | otherwise = divideWord (x:xs) c : split (dropWhile (\x -> x /= c) xs) c

    divideWord :: String -> Char -> String
    divideWord [] _ = []
    divideWord (x:xs) c | x == c = []
        | otherwise = x : divideWord xs c

    split3 :: String -> Char -> [String]
    split3 [] _ = []
    split3 (x:xs) c | x == c = split3 xs c
        | otherwise = [x]:takeWhile (/= c) xs : split3 (dropWhile (/= c) xs) c
    
    updateWord :: String -> [String]
    updateWord [] = []
    updateWord xs = map (\word -> "(" ++ word ++ ")") (split3 xs ' ')



    