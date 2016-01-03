module Assign where

import Expr

infix 1 :=
data Assign = Ident := AExpr

ppAssign (var := val) = var ++ " <= " ++ ppAExpr val

instance Show Assign where show = ppAssign

data Proc = If BExpr Proc | Proc :>> Proc | Assign Assign

ppProc (If cond proc) = "if " ++ ppBExpr cond ++ " then " ++ ppProc proc ++ " end if"
ppProc (p1 :>> p2) = ppProc p1 ++ "; " ++ ppProc p2
ppProc (Assign ass) = ppAssign ass

instance Show Proc where show = ppProc
