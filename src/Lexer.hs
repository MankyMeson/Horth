module Lexer where

lexer :: String -> String -> [String]
lexer [] stack = [reverse stack | not $ null stack]
lexer ('\"':xs) stack = lexStr xs $ '\"':stack
lexer (x:xs) stack
  | x == ' ' || x == '\n' = if null stack
    then lexer xs []
    else reverse stack:lexer xs []
  | otherwise             = lexer xs $ x:stack

lexStr :: String -> String -> [String]
lexStr ('\"':xs) stack = lexer xs $ '\"':stack
lexStr (x:xs) stack = lexStr xs $ x:stack
lexStr [] stack     = ["Nil"]