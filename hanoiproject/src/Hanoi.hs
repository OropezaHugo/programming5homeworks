module Hanoi (myHanoi) where
    import Test.QuickCheck

    myHanoi :: (Int, Char, Char, Char) -> [(Int, Char)]
    myHanoi (disks, start, aux, end) 
        | disks == 1 = [(disks, end)]
        | otherwise = myHanoi(disks - 1, start, end, aux) ++ [(disks, end)] ++ myHanoi(disks - 1, aux, start, end)

    