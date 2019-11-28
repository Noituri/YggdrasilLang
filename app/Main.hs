{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lexer
import Parser
import Text.Megaparsec

main :: IO ()
main = parseTest (parseExpr <* eof) "3 * (a + 2)"
