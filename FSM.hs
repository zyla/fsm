{-# LANGUAGE OverloadedStrings #-}
module FSM where

import Stmt

type PC = Int
type Cont = Stmt

data Seq = Seq { seqInit :: Stmt, seqCode :: PC -> Cont -> [Stmt] }

_PC = "PC"

-- sequential composition
(.>>) :: Seq -> Seq -> Seq
Seq init1 code1 .>> Seq init2 code2 = Seq
  { seqInit = init1
  , seqCode = \self1 cont2 ->
     let cont1 = init2 :| _PC := self2
         self2 = _ --self1 + length code1'
         code1' = code1 self1 cont1
     in code1' ++ code2 self2 cont2
  }
