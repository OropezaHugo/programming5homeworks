module MessageTreeFunctions where
    import Log

    insertLog :: LogMessage -> MessageTree -> MessageTree
    insertLog log Leaf = Node Leaf log Leaf
    insertLog (LogMessage ty times string) (Node leftNode (LogMessage tyroot timesroot stringroot) rightNode)
        | timesroot <= times = Node leftNode (LogMessage tyroot timesroot stringroot) (insertLog (LogMessage ty times string) rightNode)
        | timesroot > times = Node (insertLog (LogMessage ty times string) leftNode) (LogMessage tyroot timesroot stringroot) rightNode
    
    buildTree :: [LogMessage] -> MessageTree
    buildTree [] = Leaf
    buildTree (x:xs) = expandTree xs (insertLog x Leaf )

    expandTree :: [LogMessage] -> MessageTree -> MessageTree
    expandTree [] tree = tree
    expandTree (x:xs) tree = expandTree xs (insertLog x tree )

    inOrderTree :: MessageTree -> [LogMessage]
    inOrderTree Leaf = [Unknown "noMessages"]
    inOrderTree (Node leftSide root rightSide)
        | leftSide /= Leaf = inOrderTree leftSide ++ [root] ++ inOrderTree rightSide
        | otherwise = [root]


--(Node (Node Leaf (parseMessage "W 10 zxc") Leaf) (parseMessage "I 15 asda") (Node (Node Leaf (parseMessage "I 20 ipo") Leaf) (parseMessage "W 25 hjk") Leaf))

