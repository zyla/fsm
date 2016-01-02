{-# LANGUAGE DeriveFunctor #-}

import Control.Monad (ap)
import qualified Data.Map as M
import Control.Monad.State

type PC = Int

data Machine = Machine { mNextPC :: PC, mTransitions :: M.Map PC PC }

type Seq = State Machine


nextState :: Seq PC
nextState = do
  m <- get
  let pc = mNextPC m
  put $ m { mNextPC = pc + 1 }
  return pc

goto :: PC -> Seq ()
goto next = do
  pc <- get
