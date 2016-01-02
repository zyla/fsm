{-# LANGUAGE DeriveFunctor #-}

import Prelude hiding (repeat)

import Control.Monad (ap)
import qualified Data.Map as M
import Control.Monad.State

type PC = Int

type RegVal = Int
data Cond = RegEqual RegVal deriving (Show)
data Expr = RegVal | Incr Expr deriving (Show)
data Transition = Output Expr PC | If Cond Transition (Maybe Transition) deriving (Show)

type Transitions = M.Map PC Transition
data Machine = Machine { mCurrentPC :: PC, mTransitions :: Transitions } deriving (Show)

type Seq = State Machine


compile :: Seq () -> Transitions
compile seq = mTransitions $ execState seq (Machine 0 M.empty)

curState :: Seq PC
curState = gets mCurrentPC

clock :: Seq ()
clock = do
  pc <- gets mCurrentPC
  modify $ \m -> m { mCurrentPC = mCurrentPC m + 1 }

transition :: Transition -> Seq ()
transition next = do
  Machine pc transitions <- get
  put (Machine pc (M.alter (Just . appendT next) pc transitions))

goto target = transition $ Output RegVal target

appendT :: Transition -> Maybe Transition -> Transition
appendT next Nothing = next
appendT next (Just (If cond l r)) = If cond l $ Just $ appendT next r
appendT _    (Just x) = x


prog = endlessly $ do
  repeat 4 outputCommand
  repeat 12 outputCommand
  sync

outputCommand = clock
outputData = clock
sync = clock

repeat n act = do
  loop <- curState
  act
  goto loop

endlessly act = do
  loop <- curState
  act
  goto loop

render :: Transitions -> String
render transitions = header ++ concatMap trans (M.toList transitions) ++ footer
  where
    header = "digraph { rankdir=LR; size=\"8,5\"; node [shape=circle]; "
    footer = "}"
    trans (k, Output rv pc) = show k ++ "->" ++ show pc ++ " [label=\"R=" ++ show rv ++ "\"]; "
    trans (k, If cond l (Just r)) = trans (k, l) ++ trans (k, r)
    trans (k, If cond l Nothing) = trans (k, If cond l (Just $ Output RegVal (k + 1)))

main = writeFile "/tmp/fsm.gv" $ render $ compile prog
