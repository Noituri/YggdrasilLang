{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lexer
import Parser
import Text.Megaparsec

main :: IO ()
main = parseTest (parseFunction <* eof) "fc app (num Int, num2 Float) String {\
                                        \ 2 +2 \
                                        \ a + 12 \
                                        \}"