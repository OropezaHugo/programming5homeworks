import Test.QuickCheck
import Hanoi (myHanoi)

prop_hanoi :: Property
prop_hanoi = forAll (choose (1, 20)) $ \d ->
    length (myHanoi (d, 'a', 'b', 'c')) == 2^d - 1
        
main :: IO ()
main = putStrLn "Test suite not yet implemented"
