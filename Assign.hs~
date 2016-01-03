module Assign where

import Expr

infix 1 :=
data Assign = Ident := AExpr

instance Show Assign where
  show (var := val) = var ++ " <= " ++ show val
