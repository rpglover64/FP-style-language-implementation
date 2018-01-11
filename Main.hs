{-# LANGUAGE LambdaCase #-}
module Main where

data Expr = I Int | Plus Expr Expr

eval :: Expr -> Int
eval = \case
  I i -> i
  Plus e1 e2 -> eval e1 + eval e2

testExpr = (I 1 `Plus` I 2) `Plus` I 3

main = print $ eval testExpr
