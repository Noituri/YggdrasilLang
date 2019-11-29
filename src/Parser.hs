{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Text (Text, pack, unpack)
import Text.Megaparsec
import Lexer
import ASTree
import Text.Megaparsec.Char
import qualified Control.Monad.Combinators.Expr as P
import qualified Text.Megaparsec.Char.Lexer as L

parseFunction :: Parser AST
parseFunction = do
    _ <- lexeme $ string "fc"
    name <- lexeme $ some alphaNumChar
    args <- optional $ do
        _ <- lexeme $ char '('

        args' <- many $ do
            argName <- lexeme $ some alphaNumChar
            argType <- lexeme $ some alphaNumChar

            return (argName, argType)
        _ <- lexeme $ char ')'
        return args'

    return $ Function name args []

parseInteger :: Parser AST
parseInteger = Int <$> integerLex

parseFloat :: Parser AST
parseFloat = Float <$> floatLex

parseVariable :: Parser AST
parseVariable = ASTree.Var <$> variableLex

parseParens :: Parser a -> Parser a
parseParens = between (symbol "(") (symbol")")

parseTerm :: Parser AST
parseTerm = choice
  [ parseParens parseExpr
  , parseVariable
  , parseInteger
  ]

parseExpr :: Parser AST
parseExpr = P.makeExprParser parseTerm operatorTable

operatorTable :: [[P.Operator Parser AST]]
operatorTable =
  [ [ createPrefix NumNegation
    , createPrefix BoolNegation
    ]
  , [ createBinary Multiply
    , createBinary Divide
    ]
  , [ createBinary Sum
    , createBinary Sub
    ]
  ]

createBinary :: ASTree.Operator -> P.Operator Parser AST
createBinary op = P.InfixL (BinOp op <$ symbol (pack $ show op))

createPrefix, createPostfix :: ASTree.Operator -> P.Operator Parser AST
createPrefix op = P.Prefix (UnaryOp op <$ symbol (pack $ show op))
createPostfix op = P.Postfix (UnaryOp op <$ symbol (pack $ show op))