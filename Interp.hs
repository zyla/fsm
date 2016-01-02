{-# LANGUAGE DeriveFunctor #-}

import Control.Monad (ap)
import qualified Data.Map as M
import Control.Monad.State

type PC = Int

data Machine = Machine { mCurrentPC :: PC, mTransitions :: M.Map PC PC } deriving (Show)

type Seq = State Machine


compile :: Seq () -> Machine
compile seq = execState seq (Machine 0 M.empty)

nextState :: Seq PC
nextState = do
  m <- get
  let pc = mCurrentPC m + 1
  put $ m { mCurrentPC = pc }
  return pc

goto :: PC -> Seq ()
goto next = do
  Machine pc transitions <- get
  put (Machine pc (M.insert pc next transitions))


prog = do
  begin <- nextState
  nextState
  loop <- nextState
  nextState
  goto loop
  nextState
  goto begin
