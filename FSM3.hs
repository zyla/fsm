module FSM3 where

import Data.String

type Ident = String

data BExpr = Eq AExpr AExpr

data AExpr =
    Var Ident
  | Lit Int
  | Add AExpr AExpr
  | Index AExpr AExpr

ppAExpr :: AExpr -> String
ppAExpr (Var x) = x
ppAExpr (a `Index` i) = ppAExpr a ++ "[" ++ ppAExpr i ++ "]"
ppAExpr (Const x) = show x
ppAExpr (a `Add` b) = ppAExpr a ++ "+" ++ ppAExpr b

ppBExpr :: BExpr -> String
ppBExpr (a `Eq` b) = ppAExpr a ++ "=" ++ ppAExpr b

instance Show (Expr a) where show = ppExpr

instance (Num a, Show a) => Num (Expr a) where
  fromInteger = Const . fromInteger
  (+) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined
  negate = undefined
