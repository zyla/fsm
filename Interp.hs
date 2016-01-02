{-# LANGUAGE DeriveFunctor #-}

import Prelude hiding (repeat)

import Control.Monad (ap)
import qualified Data.Map as M
import Control.Monad.State

type PC = Int

data Cond = StateEqual Int
data Transition = Goto PC | If Cond Transition Transition

type Transitions = M.Map PC [PC]
data Machine = Machine { mCurrentPC :: PC, mTransitions :: Transitions } deriving (Show)

type Seq = State Machine


compile :: Seq () -> Transitions
compile seq = mTransitions $ execState seq (Machine 0 M.empty)

curState :: Seq PC
curState = gets mCurrentPC

clock :: Seq ()
clock = do
  pc <- gets mCurrentPC
  goto (pc + 1)
  modify $ \m -> m { mCurrentPC = mCurrentPC m + 1 }

goto :: PC -> Seq ()
goto next = do
  Machine pc transitions <- get
  put (Machine pc (M.insertWith (++) pc [next] transitions))


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
    trans (k, vs) = concatMap (\v -> show k ++ "->" ++ show v ++ "; ") vs

main = writeFile "/tmp/fsm.gv" $ render $ compile prog
