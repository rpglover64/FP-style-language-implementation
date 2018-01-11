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
  Minus e1 e2 -> checkArith TMinus e1 e2
  Times e1 e2 -> checkArith TTimes e1 e2
  Cond e1 e2 e3 -> do
    Tagged BTag te1 <- check e1
    Tagged t2 te2 <- check e2
    Tagged t3 te3 <- check e3
    case t2 of
      BTag -> case t3 of {BTag -> Just $ Tagged t2 $ TCond te1 te2 te3; _ -> Nothing}
      ITag -> case t3 of {ITag -> Just $ Tagged t2 $ TCond te1 te2 te3; _ -> Nothing}

checkArith :: (TExpr Int -> TExpr Int -> TExpr Int) -> Expr -> Expr -> Maybe (Tagged TExpr)
checkArith f e1 e2 = (ITag `Tagged`) <$> do
  Tagged ITag te1 <- check e1
  Tagged ITag te2 <- check e2
  return $ f te1 te2
  
