{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Main where

import Data

check :: Expr -> Maybe (Tagged TExpr)
check = \case
  B b -> Just $ BTag `Tagged` TB b
  I i -> Just $ ITag `Tagged` TI i
  Plus e1 e2 -> (ITag `Tagged`) <$> do
    Tagged ITag te1 <- check e1
    Tagged ITag te2 <- check e2
    return $ TPlus te1 te2

eval :: TExpr a -> a
eval = \case
  TB b -> b
  TI i -> i
  TPlus e1 e2 -> eval e1 + eval e2

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
