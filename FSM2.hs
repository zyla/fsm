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

type Cont = Expr (PC, RegVal)
type Machine = (Expr RegVal, Expr Output, Cont)

type Seq = (Expr RegVal, PC -> Cont -> [Machine])

sequence :: Seq -> Seq -> Seq
sequence (initial, t1) (contVal, t2) = (initial, \self finalCont ->
  let firstSeq = t1 self _
      middlePC = self + length firstSeq
  in firstSeq ++ t2 middlePC finalCont)

loop_forever :: Seq -> Seq
loop_forever (initial, trans) = (initial, \self cont -> trans self (Tuple self initial))
