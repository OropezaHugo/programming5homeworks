module Test where
    test :: [Char] -> Bool
    test ['a', _, _ ] = True
    test _ = False


    maxof3 :: Int -> Int -> Int -> Int
    maxof3 _ _ _ = 0

    parseSimpleJson :: String -> Bool
    parseSimpleJson ('{': xs : ['}'])= True

    fac :: Int -> Int
    fac n | n == 0 = 1
        | otherwise = fac (n - 1) * n
    
    tailfac :: Int -> Int -> Int
    tailfac 0 acc = acc
    tailfac n acc = tailfac (n - 1) (acc * n)

