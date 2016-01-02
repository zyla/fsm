{-# LANGUAGE ExistentialQuantification, GADTs #-}

type PC = Int
type RegVal = Int
type Input = Int
type Output = Int

data Cond = Not Cond | Eq (Expr Int) (Expr Int)

data Expr a where
  Var :: String -> Expr a
  Tuple :: Expr a -> Expr b -> Expr (a, b)
  Index :: Expr [a] -> Expr Int -> Expr a
  Const :: a -> Expr a
  Reg :: Expr RegVal
  Input :: Expr Input
  Add :: Expr a -> Expr a -> Expr a
  NC :: Expr a

  If :: Expr Bool -> Expr a -> Expr a -> Expr a


type Machine = (Expr RegVal, Expr Output, Expr (PC, RegVal))

type Seq = (Expr RegVal, PC -> Cont -> [Transition])
