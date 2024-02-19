module Hanoi where
    import Test.QuickCheck
    myHanoi :: (Int, Char, Char, Char) -> [(Int, Char)]
    myHanoi (disks, start, aux, end) 
        | disks == 1 = [(disks, end)]
        | otherwise = myHanoi(disks - 1, start, end, aux) ++ [(disks, end)] ++ myHanoi(disks - 1, aux, start, end)
    
    prop_hanoi :: Property
    prop_hanoi = forAll (choose (1, 20)) $ \d ->
        length (myHanoi (d, 'a', 'b', 'c')) == 2^d - 1


{-
    myHanoi function solves the problem recursively changing the
    start, auxiliar and end stick for its solution

    prop_hanoi function tests the myHanoi function with the min
    steps to solve them, but it uses only towers from 1 to 20 disks
    because otherwise it takes too long for the quiccheck

    How to run the code:
    enter to ghci from terminal
    :l hanoi.hs
    quickCheck prop_hanoi
     
-}