{-# LANGUAGE DeriveFunctor #-}

import Control.Monad (ap)
import qualified Data.Map as M
import Control.Monad.State

type PC = Int

type Transitions = M.Map PC [PC]
data Machine = Machine { mCurrentPC :: PC, mTransitions :: Transitions } deriving (Show)

type Seq = State Machine


compile :: Seq () -> M.Map PC PC
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


prog = do
  begin <- curState -- 0
  clock -- 1
  clock -- 2
  loop <- curState -- 2
  clock -- 4
  goto loop
  clock -- 5
  goto begin


render :: M.Map PC PC -> String
render transitions = header ++ concatMap trans (M.toList transitions) ++ footer
  where
    header = "digraph { rankdir=LR; size=\"8,5\"; node [shape=circle]; "
    footer = "}"
    trans (k, v) = show k ++ "->" ++ show v ++ ";"
