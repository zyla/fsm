module Assign where

import Expr

infix 1 :=
data Assign = Ident := AExpr

ppAssign (var := val) = var ++ " <= " ++ show val

instance Show Assign where show = ppAssign
