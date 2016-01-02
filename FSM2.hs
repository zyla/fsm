{-# LANGUAGE ExistentialQuantification #-}

type PC = Int
type RegVal = Int

data Cond = Not Cond | forall a. Eq (Expr a) (Expr a) deriving (Show)
data Expr where
  Var :: String -> Expr a
  Index :: Expr [a] -> Expr Int -> Expr a
  Const :: a -> Expr a
  Reg ::  | Input | Incr Expr | X deriving (Show)
