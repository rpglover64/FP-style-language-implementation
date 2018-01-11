{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Eval where

import Data

eval :: TExpr a -> a
eval = \case
  TB b -> b
  TI i -> i
  TPlus e1 e2 -> eval e1 + eval e2
