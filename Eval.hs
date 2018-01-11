{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Eval where

import Data

eval :: TExpr a -> a
eval = \case
  TB b -> b
  TI i -> i
  TPlus e1 e2 -> evalArith (+) e1 e2

evalArith :: (Int -> Int -> Int) -> TExpr Int -> TExpr Int -> Int
evalArith f e1 e2 = eval e1 `f` eval e2
