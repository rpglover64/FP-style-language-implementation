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
  TMinus e1 e2 -> evalArith (-) e1 e2
  TTimes e1 e2 -> evalArith (*) e1 e2
  TCond e1 e2 e3 -> if eval e1 then eval e2 else eval e3

evalArith :: (Int -> Int -> Int) -> TExpr Int -> TExpr Int -> Int
evalArith f e1 e2 = eval e1 `f` eval e2
