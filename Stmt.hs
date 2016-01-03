module Stmt where

import Expr

infix 2 :=
infix 1 :>>

data Stmt = If BExpr Stmt | Stmt :>> Stmt | Ident := AExpr

ppStmt (If cond proc) = "if " ++ ppBExpr cond ++ " then " ++ ppStmt proc ++ " end if"
ppStmt (p1 :>> p2) = ppStmt p1 ++ "; " ++ ppStmt p2
ppStmt (var := val) = var ++ " <= " ++ ppAExpr val

instance Show Stmt where show = ppStmt
