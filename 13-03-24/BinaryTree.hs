module BinaryTree where
    data Tree a = Leaf
                | Node Integer (Tree a) a (Tree a) 
                deriving (Show, Eq)

    foldTree :: [a] -> Tree a
    foldTree [] = Leaf
    foldTree (x:xs) = expandTree xs (insertLog x Leaf)


    expandTree :: [a] -> Tree a -> Tree a
    expandTree [] tree = tree
    expandTree (x:xs) tree = expandTree xs (insertLog x tree )


    insertLog :: a -> Tree a -> Tree a
    insertLog element Leaf = Node (getDepthTree Leaf) Leaf element Leaf
    insertLog element (Node n left el right) 
        | getDepthTree left < getDepthTree right = Node (1 +(getDepthTree left)) (insertLog element left) el right
        | otherwise = Node (1 + (getDepthTree right)) left el (insertLog element right)


    getDepthTree :: Tree a -> Integer
    getDepthTree Leaf = 1
    getDepthTree (Node n left el right) 
        | getDepthTree left < getDepthTree right = 1 + getDepthTree right
        | otherwise = 1 + getDepthTree left


