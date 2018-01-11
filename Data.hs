{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Data where

data Expr
  = B Bool
  | I Int
  | Plus Expr Expr
  | Minus Expr Expr
  | Times Expr Expr
  | Cond Expr Expr Expr

data TExpr a where
  TB :: Bool -> TExpr Bool
  TI :: Int -> TExpr Int
  TPlus :: TExpr Int -> TExpr Int -> TExpr Int
  TMinus :: TExpr Int -> TExpr Int -> TExpr Int
  TTimes :: TExpr Int -> TExpr Int -> TExpr Int
  TCond :: TExpr Bool -> TExpr a -> TExpr a -> TExpr a

data Tag a where
  BTag :: Tag Bool
  ITag :: Tag Int

data Tagged f = forall a. Tagged (Tag a) (f a)
