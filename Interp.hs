{-# LANGUAGE DeriveFunctor #-}

import Prelude hiding (repeat)

import Control.Monad (ap)
import qualified Data.Map as M
import Control.Monad.State

type PC = Int

type RegVal = Int
data Cond = Not Cond | Eq Expr Expr deriving (Show)
data Expr = NC String | Index Expr Expr | Const RegVal | RegVal | Input | Incr Expr | X deriving (Show)
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

dac =
  [ 0 .: output 0 0 (1, 0)
  , 1 .: repeat_upto 3
           (\index self cont -> Final (Index (NC "cmd") index) cont)
           1 -- self
           (2, 0) -- cont
  , 2 .: repeat_upto (Const 11)
           (\index self cont -> Final (Index Input index) cont)
           2 -- self
           (3, 0) -- cont
  , 3 .: Final (Const 17) (1, X) -- (Const 0) comes from 1
  ]

dac_prog =
  [ X ==> output 0
  , 0 ==> repeat_upto 3 (\index -> output (Index (NC "cmd") index))
  , 0 ==> repeat_upto 3 (\index -> output (Index Input index))
  , X ==> output 17
  ]

type SD = (Expr, PC -> (PC, Expr) -> Transition)

compile :: [SD] -> [(PC, Transition)]
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

repeat_upto' :: Expr -> (Expr -> PC -> (PC, Expr) -> Transition) -> PC -> (PC, Expr) -> Transition
repeat_upto' max act self cont = 
  If (Eq RegVal max)
   (act RegVal self cont)
   (act RegVal self (self, (Incr RegVal)))


render :: [(PC, Transition)] -> String
render transitions = header ++ concatMap (trans "") transitions ++ footer
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
showExpr X = "-"
showExpr (NC s) = s
showExpr (Index a i) = showExpr a ++ "[" ++ showExpr i ++ "]"
showExpr (Incr x) = showExpr x ++ "+1"

showCond (Not c) = "~(" ++ showCond c ++ ")"
showCond (Eq e1 e2) = showExpr e1 ++ "=" ++ showExpr e2

main = writeFile "/tmp/fsm.gv" $
    render (compile dac_prog)
   -- render dac
