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
keyword kw = string kw *> notFollowedBy alphaNumChar *> spaceConsumer

reservedKeywords :: [String]
reservedKeywords =
    [ "fc"
    , "@fc"
    ]

integerLex :: Parser Integer
integerLex = lexeme L.decimal

floatLex :: Parser Double
floatLex = lexeme L.float

identifier :: Parser String
identifier = (lexeme . try ) $ ((:) <$> letterChar <*> many alphaNumChar) >>= isReserved
    where
        isReserved w = if w `elem` reservedKeywords
                       then fail "Keyword can't be used as an identifier"
                       else return w