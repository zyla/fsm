{-# LANGUAGE DeriveFunctor #-}

import Prelude hiding (repeat)

import Control.Monad (ap)
import qualified Data.Map as M
import Control.Monad.State

type PC = Int

type RegVal = Int
data Cond = Not Cond | Eq Expr Expr deriving (Show)
data Expr = Const RegVal | RegVal | Incr Expr deriving (Show)
data Transition = Output Expr PC | If Cond Transition Transition deriving (Show)

type Machine = M.Map PC Transition

type Seq = State Machine

(.:) = (,)

dac = M.fromList
  [ 0 .: Output (Const 0) 1
  , 1 .: If (RegEqual 3)
           (Output (Const 0) 2)
           (Output (Incr RegVal) 1)
  , 2 .: If (RegEqual 11)
           (Output (Const 0) 3)
           (Output (Incr RegVal) 2)
  , 3 .: Output RegVal 0
  ]


render :: Machine -> String
render transitions = header ++ concatMap trans (M.toList transitions) ++ footer
  where
    header = "digraph { rankdir=LR; size=\"8,5\"; node [shape=circle]; "
    footer = "}"
    trans (k, Output rv pc) = show k ++ "->" ++ show pc ++ " [label=\"R<-" ++ showExpr rv ++ "\"]; "
    trans (k, If cond l r) = trans (k, l) ++ trans (k, r)

showExpr (Const x) = show x
showExpr RegVal = "R"
showExpr (Incr x) = showExpr x ++ "+1"

showCond (Not c) = "~(" ++ showCond c ++ ")"
showCond (Eq e1 e2) = showExpr e1 ++ "=" ++ showExpr e2

main = writeFile "/tmp/fsm.gv" $ render dac
