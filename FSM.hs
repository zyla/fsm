{-# LANGUAGE DataKinds, TypeOperators, GADTs, DeriveFunctor #-}
module FSM where

import Stmt

type PC = Int
type Cont = Stmt

data Seq = Seq { seqInit :: Stmt, seqCode :: PC -> Cont -> [Stmt] }
