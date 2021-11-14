module Main where

import Lib
import Lexer

main :: IO ()
main = do
    inpt <- readFile "horth.hth"
    let ast = lexer inpt []
    eval ast (HorthStack [])
