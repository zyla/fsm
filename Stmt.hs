module Stmt where

import Expr

infix 2 :=
infix 1 :>>

data Stmt =
    Stmt :| Stmt
  | Ident := AExpr
  | If BExpr Stmt
  | Case AExpr [(AExpr, Stmt)]

(.=>) = (,)

ppStmt (p1 :| p2) = ppStmt p1 ++ " " ++ ppStmt p2
ppStmt (var := val) = var ++ " <= " ++ ppAExpr val ++ ";"
ppStmt (If cond proc) = "if " ++ ppBExpr cond ++ " then " ++ ppStmt proc ++ " end if;"
ppStmt (Case expr alternatives) = "case " ++ ppAExpr expr ++ " is " ++ concatMap ppAlt alternatives ++ " end case;"
  where
    ppAlt (val, st) = "when " ++ ppAExpr val ++ " => " ++ ppStmt st

instance Show Stmt where show = ppStmt
