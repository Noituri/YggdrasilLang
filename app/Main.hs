{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lexer
import Parser
import Text.Megaparsec
import Data.Text (pack)
import Paths_YggdrasilLang (version)
import Data.Version (showVersion)

main :: IO ()
main = do
    putStrLn $ "Yggdrasil - v" ++ showVersion version
    file <- readFile "./app/tests/test1.yg"
    putStrLn "Output of the parser:"
    parseTest (parseFunction <* eof) $ pack file