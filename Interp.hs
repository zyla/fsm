{-# LANGUAGE DeriveFunctor #-}

import Prelude hiding (repeat)

import Control.Monad (ap)
import qualified Data.Map as M
import Control.Monad.State

type PC = Int

type RegVal = Int
data Cond = Not Cond | RegEqual RegVal deriving (Show)
data Expr = Const RegVal | RegVal | Incr Expr deriving (Show)
data Transition = Output Expr PC | If Cond Transition Transition deriving (Show)

type Machine = M.Map PC Transition

type Seq = State Machine


dac = M.fromList
  [ 0, Output 


render :: Transitions -> String
render transitions = header ++ concatMap trans (M.toList transitions) ++ footer
  where
    header = "digraph { rankdir=LR; size=\"8,5\"; node [shape=circle]; "
    footer = "}"
    trans (k, Output rv pc) = show k ++ "->" ++ show pc ++ " [label=\"R<-" ++ showExpr rv ++ "\"]; "
    trans (k, If cond l (Just r)) = trans (k, l) ++ trans (k, r)
    trans (k, If cond l Nothing) = trans (k, If cond l (Just $ Output RegVal (k + 1)))

showExpr RegVal = "R"
showExpr (Incr x) = showExpr x ++ "+1"

main = writeFile "/tmp/fsm.gv" $ render $ compile prog
