{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Data where

data Expr = B Bool | I Int | Plus Expr Expr

data TExpr a where
  TB :: Bool -> TExpr Bool
  TI :: Int -> TExpr Int
  TPlus :: TExpr Int -> TExpr Int -> TExpr Int

data Tag a where
  BTag :: Tag Bool
  ITag :: Tag Int

data Tagged f = forall a. Tagged (Tag a) (f a)
