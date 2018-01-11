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
  Minus e1 e2 -> printBin "-" e1 e2
  Times e1 e2 -> printBin "*" e1 e2
  Cond e1 e2 e3 -> printCond e1 e2 e3

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

printCond :: Expr -> Expr -> Expr -> String
printCond e1 e2 e3 = concat
  [ "if "
  , printExpr e1
  , " then "
  , printExpr e2
  , " else "
  , printExpr e3
  ]
