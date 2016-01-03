module Assign where

import Expr

infix 2 :=
infix 1 :>>

data Proc = If BExpr Proc | Proc :>> Proc | Ident := AExpr

ppProc (If cond proc) = "if " ++ ppBExpr cond ++ " then " ++ ppProc proc ++ " end if"
ppProc (p1 :>> p2) = ppProc p1 ++ "; " ++ ppProc p2
ppProc (Assign ass) = ppAssign ass
ppProc (var := val) = var ++ " <= " ++ ppAExpr val

instance Show Proc where show = ppProc
