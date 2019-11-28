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
    | UnaryOp Operator AST
    deriving (Eq, Ord, Show)

data Operator
    = Sum
    | Sub
    | NumNegation
    | BoolNegation
    | Multiply
    | Divide
    | Square
    | Equal
    | NEqual
    | GTE
    | LTE
    | GT
    | LT
    deriving (Eq, Ord)

instance Show Operator where
    show Sum = "+"
    show Sub = "-"
    show NumNegation = "-"
    show BoolNegation = "!"
    show Multiply = "*"
    show Divide = "/"
    show Square = "**"
    show Equal = "=="
    show NEqual = "!="
    show GTE = ">="
    show LTE = "<="
    show ASTree.GT = ">"
    show ASTree.LT = "<"