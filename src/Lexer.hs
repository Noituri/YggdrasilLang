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

keyword :: Text -> Parser ()
keyword kw = spaceConsumer *> string kw *> notFollowedBy alphaNumChar *> spaceConsumer

reservedKeywords :: [String]
reservedKeywords =
    [ "fc"
    , "@fc"
    , "true"
    , "false"
    ]

integerLex :: Parser Integer
integerLex = lexeme L.decimal

floatLex :: Parser Double
floatLex = lexeme L.float

stringLex :: Parser String
stringLex = lexeme $ char '\"' *> manyTill L.charLiteral (lexeme $ char '\"')

trueLex :: Parser Bool
trueLex = do
    keyword "true"
    return True

falseLex :: Parser Bool
falseLex = do 
    keyword "false"
    return False

boolLex :: Parser Bool
boolLex = try trueLex <|> try falseLex

identifier :: Parser String
identifier = (lexeme . try ) $ ((:) <$> letterChar <*> many alphaNumChar) >>= isReserved
    where
        isReserved w = if w `elem` reservedKeywords
                       then fail "Keyword can't be used as an identifier"
                       else return w