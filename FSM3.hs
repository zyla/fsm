module FSM3 where

import Data.String

type Ident = String

data AExpr =
  Var Ident
