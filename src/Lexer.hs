{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

integerLex :: Parser Integer
integerLex = lexeme L.decimal

floatLex :: Parser Double
floatLex = lexeme L.float

-- TODO: Use Megaparsec instead of parsec
-- lexer :: Tok.TokenParser ()
-- lexer = Tok.makeTokenParser emptyDef {
--       Tok.commentLine = "//"
--     , Tok.commentStart = "/*"
--     , Tok.commentEnd = "*/"
--     , Tok.reservedOpNames = ["+", "-", "*", "/", "**", "==", "!=", ">=", "<=", ">", "<"]
--     , Tok.reservedNames = ["fc", "@fc", "if", "else", "loop"]
--     , Tok.caseSensitive = True
-- }

-- integer :: Parser Integer
-- integer = Tok.integer lexer

-- float :: Parser Double
-- integer = Tok.float lexer

-- parens :: Parser a -> Parser a
-- integer = Tok.parens lexer

-- commaSeparator :: Parser a -> Parser [a]
-- commaSeparator = Tok.commaSep lexer

-- semiSep :: Parser a -> Parser [a]
-- semiSep = Tok.semiSep lexer

-- identifier :: Parser String
-- identifier = Tok.identifier lexer

-- reserved :: String -> Parser ()
-- reserved = Tok.reserved lexer

-- reservedOp :: String -> Parser ()
-- reservedOp = Tok.reservedOp lexer