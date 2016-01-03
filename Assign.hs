module Assign where

import Expr

infix 1 :=
data Assign = Ident := AExpr

ppAssign (var := val) = var ++ " <= " ++ ppAExpr val

instance Show Assign where show = ppAssign

data Proc = If BExpr Proc | Proc :>> Proc | Assign Assign
