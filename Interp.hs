{-# LANGUAGE DeriveFunctor #-}

import Prelude hiding (repeat)

import Control.Monad (ap)
import qualified Data.Map as M
import Control.Monad.State

type PC = Int

type RegVal = Int
data Cond = Not Cond | Eq Expr Expr deriving (Show)
data Expr = NC String | Index Expr Expr | Const RegVal | RegVal | Input | Incr Expr deriving (Show)
data Transition = Next Expr Expr PC | If Cond Transition Transition deriving (Show)

type Machine = M.Map PC Transition

type Seq = State Machine

(.:) = (,)

stateNames = [ "Init", "Cmd", "Data", "Sync" ]

dac = M.fromList
  [ 0 .: Next (Const 0) (Const 0) 1
  , 1 .: If (Eq RegVal (Const 3))
           (Next (Index (NC "cmd") RegVal) (Const 0) 2) -- (Const 0) comes from 2
           (Next (Index (NC "cmd") RegVal) (Incr RegVal) 1)
  , 2 .: If (Eq RegVal (Const 11))
           (Next (Index Input RegVal) (Const 0) 3) -- (Const 0) comes from 3)
           (Next (Index Input RegVal) (Incr RegVal) 2)
  , 3 .: Next (Const 0) (Const 0) 1 -- (Const 0) comes from 1
  ]


render :: Machine -> String
render transitions = header ++ concatMap (trans "") (M.toList transitions) ++ footer
  where
    header = "digraph { rankdir=LR; size=\"8,5\"; node [shape=circle]; "
    trans conds (k, Next output rv pc) =
      showPC k ++ "->" ++ showPC pc ++ " [label=\"" ++ conds ++
        "O<-" ++ showExpr output ++ ";\\n " ++
        "R<-" ++ showExpr rv ++ "\"]; "
    trans conds (k, If cond l r) =
      let cstr = conds ++ showCond cond ++ " => "
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
