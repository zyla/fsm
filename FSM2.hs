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
  Const :: Show a => a -> Expr a
  Reg :: Expr RegVal
  PC :: Expr PC
  Input :: Expr Input
  Add :: Expr a -> Expr a -> Expr a
  NC :: Expr a

  If :: Expr Bool -> Expr a -> Expr a -> Expr a


instance (Num a, Show a) => Num (Expr a) where
  fromInteger = Const . fromInteger
  (+) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined
  negate = undefined


type Cont = Expr (PC, RegVal)
type Machine = (Expr RegVal, Expr Output, Cont)

type Seq = (Expr RegVal, PC -> Cont -> [Machine])

sequence :: Seq -> Seq -> Seq
sequence (initial, t1) (contVal, t2) = (initial, \self finalCont ->
  let firstSeq = t1 self (Tuple (Const middlePC) contVal)
      middlePC = self + length firstSeq
  in firstSeq ++ t2 middlePC finalCont)

loop_forever :: Seq -> Seq
loop_forever (initial, trans) = (initial, \self cont -> trans self (Tuple (Const self) initial))

instantiate :: Seq -> (Expr RegVal, PC, [(PC, Machine)])
instantiate (initial, m) = (initial, 0, zip [0..] (m 0 (Tuple 0 initial)))


compileSwitch :: [(PC, Machine)] -> Machine
compileSwitch ((pc, (regval, output, cont):xs) =
  let switch = If (Eq PC (Const pc))
      (regval', output', cont') = compileSwitch xs
  in (switch regval regval', switch output output', switch cont cont')
