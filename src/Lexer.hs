module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok
-- TODO: Use Megaparsec instead of parsec
lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser emptyDef {
      Tok.commentLine = "//"
    , Tok.commentStart = "/*"
    , Tok.commentEnd = "*/"
    , Tok.reservedOpNames = ["+", "-", "*", "/", "**", "==", "!=", ">=", "<=", ">", "<"]
    , Tok.reservedNames = ["fc", "@fc", "if", "else", "loop"]
    , Tok.caseSensitive = True
}

integer :: Parser Integer
integer = Tok.integer lexer

float :: Parser Double
integer = Tok.float lexer

parens :: Parser a -> Parser a
integer = Tok.parens lexer

commaSeparator :: Parser a -> Parser [a]
commaSeparator = Tok.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer