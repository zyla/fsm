module Stmt where

import Expr

infix 2 :=
infixl 1 :|

data Stmt =
    Stmt :| Stmt -- parallel composition
  | Ident := AExpr -- assignment
  | If BExpr Stmt Stmt
  | Case AExpr [(AExpr, Stmt)]
  | Nop

(.=>) = (,)

ppStmt (Nop :| p) = ppStmt p
ppStmt (p :| Nop) = ppStmt p
ppStmt (p1 :| p2) = ppStmt p1 ++ "\n" ++ ppStmt p2
ppStmt (var := val) = var ++ " <= " ++ ppAExpr val ++ ";"
ppStmt (If cond iftrue iffalse) = "if " ++ ppBExpr cond ++ " then " ++ ppStmt iftrue ++ " else " ++ ppStmt iffalse ++ " end if;"
ppStmt (Case expr alternatives) = "case " ++ ppAExpr expr ++ " is " ++ concatMap ppAlt alternatives ++ " end case;"
  where
    ppAlt (val, st) = "when " ++ ppAExpr val ++ " => " ++ ppStmt st ++ "\n"
ppStmt Nop = ";"

instance Show Stmt where show = ppStmt
