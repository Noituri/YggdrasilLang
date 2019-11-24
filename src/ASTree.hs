module ASTree where

data AST
    = Float Double
    | Int Integer
    | Function String [AST] AST
    | ExternFunc String [AST]
    | Call String [AST]
    | Var String
    | BinOp Operator AST AST
    deriving (Eq, Ord, Show)

data Operator
    = Plus
    | Minus
    | Multiply
    | Divide
    | Square
    | Equal
    | NEqual
    | GTE
    | LTE
    | GT
    | LT
    deriving (Eq, Ord, Show)