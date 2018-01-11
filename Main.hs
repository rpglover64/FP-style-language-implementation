{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Main where

import Data
import Check
import Eval
import Print

testExpr1 = (I 1 `Plus` I 2) `Plus` I 3
testExpr2 = (I 1 `Plus` I 2) `Plus` B True
testExpr3 = B True

printEval :: Expr -> IO ()
printEval e = print (printExpr e) >> case te of
  Nothing -> print "type error"
  Just te -> case te of
    Tagged BTag e -> print $ eval e
    Tagged ITag e -> print $ eval e
  where
    te = check e

main = mapM_ printEval [testExpr1, testExpr2, testExpr3]
