{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Text (Text, pack, unpack)
import Text.Megaparsec
import Lexer
import ASTree
import Text.Megaparsec.Char
import qualified Control.Monad.Combinators.Expr as P
import qualified Text.Megaparsec.Char.Lexer as L

parseTopLevel :: Parser [AST]
parseTopLevel = many $ try parseFunction <|> try parseExternFunction
    
parseArgs :: Parser (Name, Type)
parseArgs = do
    argName <- lexeme (some alphaNumChar <?> "arg name")
    argType <- lexeme (some alphaNumChar <?> "arg type")
    return (argName, argType)

parseFunction :: Parser AST
parseFunction = do
    reserve "fc"
    name <- lexeme $ some alphaNumChar
    args <- optional . try $ do
        _ <- lexeme $ char '('
        args' <- sepBy parseArgs (symbol ",")
        _ <- lexeme $ char ')'
        return args'
    returnType <- optional . try $ lexeme (some alphaNumChar <?> "function return type")
    _ <- lexeme $ char '{'
    body <- many parseExpr
    _ <- lexeme $ char '}'
    return $ Function name args returnType body

parseExternFunction :: Parser AST
parseExternFunction = do
    reserve "@fc"
    name <- lexeme $ some alphaNumChar
    args <- optional . try $ do
        _ <- lexeme $ char '('
        args' <- sepBy parseArgs (symbol ",")
        _ <- lexeme $ char ')'
        return args'
    returnType <- optional . try $ lexeme (some alphaNumChar <?> "extern function return type")
    return $ ExternFunc name args returnType

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