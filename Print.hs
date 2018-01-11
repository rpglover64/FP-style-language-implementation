{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Print where

import Data

printExpr :: Expr -> String
printExpr = \case
  B b -> show b
  I i -> show i
  Plus e1 e2 -> printBin "+" e1 e2

printBin :: String -> Expr -> Expr -> String
printBin x e1 e2 = concat
  [ "("
  , printExpr e1
  , ")"
  , " "
  , x
  , " "
  , "("
  , printExpr e2
  , ")"
  ]
