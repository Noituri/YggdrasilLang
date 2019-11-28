{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lexer
import Text.Megaparsec

main :: IO ()
main = parseTest (integerLex <* eof) "123"
