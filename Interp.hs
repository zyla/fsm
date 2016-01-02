{-# LANGUAGE DeriveFunctor #-}

import Prelude hiding (repeat)

import Control.Monad (ap)
import qualified Data.Map as M
import Control.Monad.State

type PC = Int

type RegVal = Int
data Cond = Not Cond | Eq Expr Expr deriving (Show)
data Expr = NC String | Index Expr Expr | Const RegVal | RegVal | Input | Incr Expr deriving (Show)
data Transition = Final Expr (PC, Expr) | If Cond Transition Transition deriving (Show)

type Machine = M.Map PC Transition

type Seq = State Machine

instance Num Expr where
  fromInteger = Const . fromInteger
  (+) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined
  negate = undefined

(.:) = (,)
(==>) = (,)

stateNames = [ "Init", "Cmd", "Data", "Sync" ]

dac = M.fromList
  [ 0 .: output (Const 0) 0 (1, (Const 0))
  , 1 .: repeat_upto (Const 3)
           (\index self cont -> Final (Index (NC "cmd") index) cont)
           1 -- self
           (2, Const 0) -- cont
  , 2 .: repeat_upto (Const 11)
           (\index self cont -> Final (Index Input index) cont)
           2 -- self
           (3, Const 0) -- cont
  , 3 .: Final (Const 0) (1, (Const 0)) -- (Const 0) comes from 1
  ]

dac_prog =
  [ 0 ==> output 0
  , 0 ==> repeat_upto 3 (\index -> output (Index (NC "cmd") index))
  ]

compile :: [(Expr, PC -> (PC, Expr) -> Transition)] -> [(PC, Transition)]
compile prog =
  let concreteStates = zipWith (\index (initial, tr) -> ((index, initial), tr index)) [0..] prog
  in cycleZipWith (\((index, _), tr) (cont, _) -> (index, tr cont)) concreteStates

cycleZipWith :: (a -> a -> b) -> [a] -> [b]
cycleZipWith _ [] = []
cycleZipWith f (hd:xs) = go (hd:xs)
  where
    go (x:y:xs) = f x y : go (y:xs)
    go [x] = [f x hd]

output :: Expr -> PC -> (PC, Expr) -> Transition
output out self cont = Final out cont

repeat_upto :: Expr -> (Expr -> PC -> (PC, Expr) -> Transition) -> PC -> (PC, Expr) -> Transition
repeat_upto max act self cont = 
  If (Eq RegVal max)
   (act RegVal self cont)
   (act RegVal self (self, (Incr RegVal)))


render :: Machine -> String
render transitions = header ++ concatMap (trans "") (M.toList transitions) ++ footer
  where
    header = "digraph { rankdir=LR; size=\"8,5\"; node [shape=circle]; "
    trans conds (k, Final output (pc, rv)) =
      showPC k ++ "->" ++ showPC pc ++ " [label=\"" ++ conds ++
        "↑" ++ showExpr output ++ "\\n " ++
        "R←" ++ showExpr rv ++ "\"]; "
    trans conds (k, If cond l r) =
      let cstr = conds ++ "if " ++ showCond cond ++ "\\n"
      in trans cstr (k, l) ++ trans conds (k, r)
    footer = "}"


showPC = (stateNames !!)

showExpr (Const x) = show x
showExpr RegVal = "R"
showExpr Input = "I"
showExpr (NC s) = s
showExpr (Index a i) = showExpr a ++ "[" ++ showExpr i ++ "]"
showExpr (Incr x) = showExpr x ++ "+1"

showCond (Not c) = "~(" ++ showCond c ++ ")"
showCond (Eq e1 e2) = showExpr e1 ++ "=" ++ showExpr e2

main = writeFile "/tmp/fsm.gv" $ render dac
