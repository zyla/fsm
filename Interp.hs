{-# LANGUAGE DeriveFunctor #-}

import Control.Monad (ap)
import qualified Data.Map as M
import Control.Monad.State

type PC = Int

data Machine = Machine { mCurrentPC :: PC, mTransitions :: M.Map PC PC } deriving (Show)

type Seq = State Machine


compile :: Seq () -> Machine
compile seq = execState seq (Machine 0 M.empty)

curState :: Seq PC
curState = gets mCurrentPC

clock :: Seq ()
clock = modify $ \m -> m { mCurrentPC = mCurrentPC m + 1 }

goto :: PC -> Seq ()
goto next = do
  Machine pc transitions <- get
  put (Machine pc (M.insert pc next transitions))


prog = do
  begin <- curState -- 0
  clock
  clock -- 2
  loop <- nextState -- 3
  nextState -- 4
  goto loop
  nextState -- 5
  goto begin


render :: M.Map PC PC -> String
render transitions = header ++ concatMap trans (M.toList transitions) ++ footer
  where
    header = "digraph { rankdir=LR; size=\"8,5\"; node [shape=circle]; "
    footer = "}"
    trans (k, v) = show k ++ "->" show v ++ ";"
