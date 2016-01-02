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

nextState :: Seq ()
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
  begin <- nextState -- 1
  nextState -- 2
  loop <- nextState -- 3
  nextState -- 4
  goto loop
  nextState -- 5
  goto begin
