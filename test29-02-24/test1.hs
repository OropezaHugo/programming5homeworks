module Test1 where

    tpl :: [String] -> (String, Int)
    tpl [] = ("", 0)
    tpl [x, y] = (x, read y)

    tpl (x:xs) = ("", 0)

    removecomas :: String -> String
    removecomas [] = []
    removecomas (x:xs) 
        | x == ',' = removecomas xs
        | otherwise = x : removecomas xs

    tuplalizelist :: [String] -> [(String, Int)]
    tuplalizelist [] = []
    tuplalizelist (x:y:xs)
        | xs /= [] = [tpl ([x] ++ [y])] ++ tuplalizelist (xs)
        | otherwise = [tpl ([x] ++ [y])]
    
    tuplalizer :: String -> [(String, Int)]
    tuplalizer [] = []
    tuplalizer text = tuplalizelist (words (removecomas text))

    sumage :: [(String, Int)] -> Int
    sumage [] = 0
    sumage ((name, age):xs)
        | xs /= [] =  age + (sumage xs)
        | otherwise = age

    averagetu :: [(String, Int)] -> Int
    averagetu [] = 0
    averagetu lista = (sumage lista)  `div` (length lista)


    names :: [(String, Int)] -> [String]
    names [] = [""]
    names ((name, age):xs)
        | xs /= [] = [name] ++ names xs
        | otherwise = [name]

    lessthan :: [(String, Int)] -> Int -> [String]
    lessthan [] _ = []
    lessthan ((x,y): xs) n
        | y < n && xs /= [] = [x] ++ lessthan xs n
        | xs /= [] = lessthan xs n
        | y < n = [x]
        | otherwise = []

    isinrange :: [(String, Int)] -> Int -> Int -> [(String, Int)]
    isinrange [] _ _ = []
    isinrange ((x,y): xs) min max
        | y <= max && y >= min && xs /= [] = [(x, y)] ++ isinrange xs min max
        | xs /= [] = isinrange xs min max
        | y <= max && y >= min = [(x, y)]
        | otherwise = []

    exercise1 :: String -> Int
    exercise1 [] = 0
    exercise1 text = averagetu (tuplalizer text)

    exercise2 :: String -> [String]
    exercise2 [] = []
    exercise2 text = names (tuplalizer text)

    exercise3 :: String -> Int -> [String]
    exercise3 [] _ = []
    exercise3 text number = lessthan (tuplalizer text) number

    exercise4 :: String -> Int -> Int -> [(String, Int)]
    exercise4 [] _ _= []
    exercise4 text number1  number2 = isinrange (tuplalizer text) number1 number2
