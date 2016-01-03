module Assign where

import Expr

infix 1 :=
data Assign = Ident := AExpr deriving (Show)
