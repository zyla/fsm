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
  X :: Expr a

  If :: Cond -> Expr a -> Expr a -> Expr a


instance (Num a, Show a) => Num (Expr a) where
  fromInteger = Const . fromInteger
  (+) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined
  negate = undefined


type Cont = Expr (PC, RegVal)
type Machine = (Expr RegVal, PC, Expr Output, Cont)

type Seq = (Expr RegVal, PC -> Cont -> [(Expr Output, Cont)])

sequence :: Seq -> Seq -> Seq
sequence (initial, t1) (contVal, t2) = (initial, \self finalCont ->
  let firstSeq = t1 self (Tuple (Const middlePC) contVal)
      middlePC = self + length firstSeq
  in firstSeq ++ t2 middlePC finalCont)

loop_forever :: Seq -> Seq
loop_forever (initial, trans) = (initial, \self cont -> trans self (Tuple (Const self) initial))

instantiate :: Seq -> (Expr RegVal, PC, [(PC, (Expr Output, Cont))])
instantiate (initial, m) = (initial, 0, zip [0..] (m 0 (Tuple 0 initial)))

compileSwitch :: Expr RegVal -> PC -> [(PC, (Expr Output, Cont))] -> Machine
compileSwitch initialRegval initialPC ((pc, (output, cont)):xs) =
  let switch :: Expr a -> Expr a -> Expr a
      switch = If (Eq PC (Const pc))
      (_, _, output', cont') = compileSwitch initialRegval initialPC xs
  in (initialRegval, initialPC, switch output output', switch cont cont')


output :: Expr Output -> Seq
output out = (X, \self cont -> [(out, cont)])

loop_from_to :: RegVal -> RegVal -> (Expr RegVal -> Seq) -> Seq
loop_from_to from to actF =
  let (_, act) = actF RegVal
  _
