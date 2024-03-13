module MessageTreeFunctions where
    import Log

    insertLog :: Maybe LogMessage -> Maybe MessageTree -> Maybe MessageTree
    insertLog (Just log) (Just Leaf) = Just (Node (Just Leaf) (Just log) (Just Leaf))
    insertLog (Just (LogMessage ty times string))
        (Just (Node leftNode (Just (LogMessage tyroot timesroot stringroot)) rightNode))
        | timesroot <= times = Just (Node leftNode (Just (LogMessage tyroot timesroot stringroot)) (insertLog (Just (LogMessage ty times string)) rightNode))
        | timesroot > times = Just (Node (insertLog (Just (LogMessage ty times string)) leftNode) (Just (LogMessage tyroot timesroot stringroot)) rightNode)
    
    buildTree :: [Maybe LogMessage] -> Maybe MessageTree
    buildTree [Nothing] = Just Leaf
    buildTree [] = Just Leaf
    buildTree (x:xs) = expandTree xs (insertLog x (Just Leaf) )

    expandTree :: [Maybe LogMessage] -> Maybe MessageTree -> Maybe MessageTree
    expandTree [Nothing] tree = tree
    expandTree [] tree = tree
    expandTree (x:xs) tree = expandTree xs (insertLog x tree )

    inOrderTree :: Maybe MessageTree -> [Maybe LogMessage]
    inOrderTree Nothing = [Just (Unknown (Just "noMessages"))]
    inOrderTree (Just Leaf) = [Just (Unknown (Just "noMessages"))]
    inOrderTree (Just (Node leftSide root rightSide))
        | leftSide /= (Just Leaf) = inOrderTree leftSide ++ [root] ++ inOrderTree rightSide
        | otherwise = [root]


--(Node (Node Leaf (parseMessage "W 10 zxc") Leaf) (parseMessage "I 15 asda") (Node (Node Leaf (parseMessage "I 20 ipo") Leaf) (parseMessage "W 25 hjk") Leaf))

