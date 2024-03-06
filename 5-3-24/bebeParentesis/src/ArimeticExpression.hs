module ArimeticExpression (Expression(..)) where
    data Expression =
        Val Int 
        | Sum Expression Expression 
        | Mult Expression Expression
        | Div Expression Expression
        | Pow Expression Expression deriving (Show)


    

