-- program transforms expression from infix notation to some example of prefix notation

{-# LANGUAGE LambdaCase #-}

module Main where

import Parser

showExpr :: Expr -> String
showExpr = \case
  a :& b -> "(&," ++ showExpr a ++ "," ++ showExpr b ++ ")"
  a :| b -> "(|," ++ showExpr a ++ "," ++ showExpr b ++ ")"
  a :-> b -> "(->," ++ showExpr a ++ "," ++ showExpr b ++ ")"
  ExprNeg a -> "(!" ++ showExpr a ++ ")"
  ExprVar s -> s

main :: IO ()
main = do
  input <- getLine
  case runParser parseFile input of
    Just (expr, []) -> putStrLn $ showExpr expr
    _ -> error "Parse error"