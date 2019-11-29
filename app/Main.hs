{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lexer
import Parser
import Text.Megaparsec

main :: IO ()
main = parseTest (parseExternFunction <* eof) "@fc app"