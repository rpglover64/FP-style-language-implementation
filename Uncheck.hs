{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Uncheck where

import Data

uncheck :: TExpr a -> Expr
uncheck = \case
  TB b -> B b
  TI i -> I i
  TPlus e1 e2 -> uncheck e1 `Plus` uncheck e2
  TCond e1 e2 e3 -> Cond (uncheck e1) (uncheck e2) (uncheck e3)
  
