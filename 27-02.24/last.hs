module Last where
    last1 :: Num a => [a] -> a
    last1 [] =  0
    last1 (x:xs)
        | null xs = x
        | otherwise  = last1 xs
