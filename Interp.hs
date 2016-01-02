{-# LANGUAGE DeriveFunctor #-}

import Control.Monad (ap)
import qualified Data.Map as M
import Control.Monad.State

type PC = Int



type Seq = State Machine

nextState :: Seq PC
nextState = Seq $ \pc -> (pc, pc + 1)

goto :: PC -> Seq ()
goto pc = Seq $ \_ -> ((), pc)
