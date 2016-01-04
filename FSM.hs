{-# LANGUAGE DataKinds, TypeOperators, GADTs, DeriveFunctor #-}
module FSM where

import Stmt

type PC = Int
type Cont = Stmt

data Seq = Seq { seqInit :: Stmt, seqCode :: PC -> Cont -> [Stmt] }

-- sequential composition
(:>>) :: Seq -> Seq -> Seq
Seq init1 code1 :>> Seq init2 code2
