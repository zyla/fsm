{-# LANGUAGE DeriveFunctor #-}

import Control.Monad (ap)
import qualified Data.Map as M
import Control.Monad.State

type PC = Int

type Machine = M.Map PC PC

type Seq = State Machine


nextState :: Seq PC
nextState = do
  pc <- get
  put (pc + 1)
  return pc

goto :: PC -> Seq ()
goto pc = Seq $ \_ -> ((), pc)
