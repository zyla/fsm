module FSM3 where

import Data.String

type Ident = String

data AExpr =
    Var Ident
  | Lit Int
  | Add AExpr AExpr
  | AExpr :! AExpr

data BExpr = AExpr := AExpr

ppAExpr :: AExpr -> String
ppAExpr (Var x) = x
ppAExpr (Lit x) = show x
ppAExpr (a :! i) = ppAExpr a ++ "[" ++ ppAExpr i ++ "]"
ppAExpr (a `Add` b) = ppAExpr a ++ "+" ++ ppAExpr b

ppBExpr :: BExpr -> String
ppBExpr (a =:= b) = ppAExpr a ++ "=" ++ ppAExpr b

instance Show AExpr where show = ppAExpr
instance Show BExpr where show = ppBExpr

instance IsString AExpr where
  fromString = Var

instance Num AExpr where
  fromInteger = Lit . fromInteger
  (+) = Add

  (*) = undefined
  abs = undefined
  signum = undefined
  negate = undefined
