module ASTree where

type Name = String

data AST
    = Float Double
    | Int Integer
    | String String
    | Bool Bool
    | Function String [AST] AST
    | ExternFunc String [AST]
    | Call String [AST]
    | Var Name
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