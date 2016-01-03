module Assign where

import Expr

data Assign = Ident := AExpr deriving (Show

infixl 1 :=)
