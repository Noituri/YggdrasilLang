{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lexer
import Parser
import Text.Megaparsec
import Data.Text (pack)

main :: IO ()
main = do
    file <- readFile "./app/tests/test1.yg"
    parseTest (parseTopLevel <* eof) $ pack file