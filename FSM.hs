{-# LANGUAGE OverloadedStrings #-}
module FSM where

import Stmt

type PC = Int
type Cont = Stmt

data Seq = Seq { seqInit :: Stmt, seqCode :: PC -> Cont -> [Stmt] }

_PC = "PC"

-- sequential composition
(:>>) :: Seq -> Seq -> Seq
Seq init1 code1 :>> Seq init2 code2 = Seq
  { seqInit = init1
  , seqCode = \self cont ->
     let cont' = init2 :| _PC
     code1 self
