{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Main where

data Expr = I Int | Plus Expr Expr

data TExpr a where
  TI :: Int -> TExpr Int
  TPlus :: TExpr Int -> TExpr Int -> TExpr Int

check :: Expr -> TExpr Int
check = \case
  I i -> TI i
  Plus e1 e2 -> TPlus (check e1) (check e2)

eval :: TExpr a -> a
eval = \case
  TI i -> i
  TPlus e1 e2 -> eval e1 + eval e2

testExpr = (I 1 `Plus` I 2) `Plus` I 3

main = print $ eval $ check testExpr
