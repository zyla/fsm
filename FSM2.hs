{-# LANGUAGE GADTs #-}

type PC = Int
type RegVal = Int
type Input = Int

data Cond = Not Cond | forall a. Eq (Expr a) (Expr a) deriving (Show)
data Expr a where
  Var :: String -> Expr a
  Index :: Expr [a] -> Expr Int -> Expr a
  Const :: a -> Expr a
  Reg :: Expr RegVal
  Input :: Expr Input
  Add :: Expr a -> Expr a -> Expr a
  X :: Expr a
  deriving (Show)
