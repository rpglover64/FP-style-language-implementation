{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Main where

import Data
import Check
import Eval

testExpr1 = (I 1 `Plus` I 2) `Plus` I 3
testExpr2 = (I 1 `Plus` I 2) `Plus` B True
testExpr3 = B True

printEval :: Maybe (Tagged TExpr) -> IO ()
printEval = \case
  Nothing -> print "type error"
  Just te -> case te of
    Tagged BTag e -> print $ eval e
    Tagged ITag e -> print $ eval e

main = mapM_ (printEval . check) [testExpr1, testExpr2, testExpr3]
