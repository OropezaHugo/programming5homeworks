module ApplicativeHomeWork2 where
    import Control.Applicative ()
    --1
    data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show
    instance Functor Tree where
        fmap :: (a -> b) -> Tree a -> Tree b
        fmap g Leaf = Leaf 
        fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)
    --2
    --instance Functor ((->) a) where
        --fmap f g = \x -> f (g x)
    

    --3
    --instance Applicative ((->) a) where
        --f <*> g = \x -> f x (g x)
        --pure x _ = x
    
    --5
    --the types that accomplish the 4 Applicative laws are
    -- Monoid a
    -- (Monoid a, Monoid b)
    -- (Monoid a, Monoid b, Monoid c)
    -- ((->) r)
    -- IO
    -- Maybe
    -- Solo
    -- []
    -- (Either e)



    