module Init where
    init1 :: Num a => [a] -> [a]
    init1 [] = []
    init1 (x:xs)
        | null xs = []
        | otherwise = [x] ++ init1 xs

    init2 :: Num a => [a] -> [a]
    init2 [] = []
    init2 li = reverse (drop 1 (reverse li))
