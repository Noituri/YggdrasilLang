{-# LANGUAGE OverloadedStrings #-}

module Parsers where

import Data.Text (Text)
import Text.Megaparsec
import Lexer
import ASTree
import qualified Control.Monad.Combinators.Expr as Pc

parseInteger :: Parser AST
parseInteger = Int <$> integerLex

parseTerm :: Parser AST
parseTerm = choice
  [ parseInteger
  ]

parseExpr :: Parser AST
parseExpr = Pc.makeExprParser parseTerm operatorTable

operatorTable :: [[Pc.Operator Parser AST]]
operatorTable =
  [ [ prefix NumNegation
    , prefix BoolNegation
    ]
  , [ Parsers.binary Multiply
    , Parsers.binary Divide
    ]
  , [ Parsers.binary Sum
    , Parsers.binary Sub
    ]
  ]

binary :: ASTree.Operator -> Pc.Operator Parser AST
binary op = Pc.InfixL (BinOp <$ symbol "+")

prefix, postfix :: ASTree.Operator -> Pc.Operator Parser AST
prefix op = Pc.Prefix (UnaryOp <$ symbol . show op)
postfix op = Pc.Postfix (UnaryOp <$ symbol . show op)