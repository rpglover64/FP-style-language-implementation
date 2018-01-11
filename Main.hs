{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Main where

import Data
import Check
import Uncheck
import Eval
import Print

testExpr1 = (I 1 `Plus` I 2) `Plus` I 3
testExpr2 = (I 1 `Plus` I 2) `Plus` B True
testExpr3 = B True

printEval :: Expr -> IO ()
printEval e = do
  putStrLn "--------"
  print $ printExpr e
  mapM_ (print . printExpr) ue
  case te of
    Nothing -> print "type error"
    Just te -> case te of
      Tagged BTag e -> print $ eval e
      Tagged ITag e -> print $ eval e
  where
    te = check e
    ue = fmap (\(Tagged _ e) -> uncheck e) te

main = mapM_ printEval [testExpr1, testExpr2, testExpr3]
