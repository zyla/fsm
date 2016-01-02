{-# LANGUAGE ExistentialQuantification #-}

type PC = Int

type RegVal = Int
data Cond = Not Cond | forall a. Eq (Expr a) (Expr a) deriving (Show)
data Expr = NC String | Index Expr Expr | Const RegVal | RegVal | Input | Incr Expr | X deriving (Show)
