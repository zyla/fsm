{-# LANGUAGE DeriveFunctor #-}

import Control.Monad (ap)
import qualified Data.Map as M
import Control.Monad.State

type PC = Int

data Machine = Machine { mCurrentPC :: PC, mTransitions :: M.Map PC PC }

type Seq = State Machine


nextState :: Seq PC
nextState = do
  pc <- get
  put (pc + 1)
  return pc

goto :: PC -> Seq ()
goto next = do
  pc <- get
