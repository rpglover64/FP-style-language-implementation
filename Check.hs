{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Check where

import Data

check :: Expr -> Maybe (Tagged TExpr)
check = \case
  B b -> Just $ BTag `Tagged` TB b
  I i -> Just $ ITag `Tagged` TI i
  Plus e1 e2 -> checkArith TPlus e1 e2

checkArith :: (TExpr Int -> TExpr Int -> TExpr Int) -> Expr -> Expr -> Maybe (Tagged TExpr)
checkArith f e1 e2 = (ITag `Tagged`) <$> do
  Tagged ITag te1 <- check e1
  Tagged ITag te2 <- check e2
  return $ f te1 te2
  
