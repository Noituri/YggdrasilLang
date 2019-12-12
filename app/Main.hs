{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lexer
import Parser
import Codegen
import Text.Megaparsec
import Data.Text (pack)
import Paths_YggdrasilLang (version)
import Data.Version (showVersion)

main :: IO ()
main = do
    -- TODO add cli
    putStrLn $ "Yggdrasil - v" ++ showVersion version
    file <- readFile "./app/tests/test1.yg"
    putStrLn "Output of the parser:"
    parseTest (parseTopLevel <* eof) $ pack file