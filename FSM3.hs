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
ppAExpr (Lit x) = show x
ppAExpr (a `Index` i) = ppAExpr a ++ "[" ++ ppAExpr i ++ "]"
ppAExpr (a `Add` b) = ppAExpr a ++ "+" ++ ppAExpr b

ppBExpr :: BExpr -> String
ppBExpr (a `Eq` b) = ppAExpr a ++ "=" ++ ppAExpr b

instance Show AExpr where
  show = ppAExpr

instance Num AExpr where
  fromInteger = Lit . fromInteger
  (+) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined
  negate = undefined
