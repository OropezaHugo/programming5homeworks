module ApplicativeHomwork where
    --4
    newtype ZipList a = Z [a] deriving Show
    instance Functor ZipList where
    -- fmap :: (a -> b) -> ZipList a -> ZipList b
        fmap g (Z xs) =Z (g (head xs) : fmap g (tail xs))
    instance Applicative ZipList where
    -- pure :: a -> ZipList a
        pure a = Z [a]
    -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
        (Z gs) <*> (Z xs) = Z [g x | g <- gs, x <- xs]
