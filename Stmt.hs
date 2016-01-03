module Stmt where

import Expr

infix 2 :=
infix 1 :>>

data Stmt =
    Stmt :>> Stmt
  | Ident := AExpr
  | If BExpr Stmt
  | Case [(AExpr, Stmt)]

ppStmt (If cond proc) = "if " ++ ppBExpr cond ++ " then " ++ ppStmt proc ++ " end if"
ppStmt (p1 :>> p2) = ppStmt p1 ++ "; " ++ ppStmt p2
ppStmt (var := val) = var ++ " <= " ++ ppAExpr val

instance Show Stmt where show = ppStmt
