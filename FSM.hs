{-# LANGUAGE OverloadedStrings #-}
module FSM where

import Expr
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
     let cont1 = init2 :| _PC := Lit self2
         self2 = self1 + length code1'
         code1' = code1 self1 cont1
     in code1' ++ code2 self2 cont2
  }

seqs :: [Seq] -> Seq
seqs = foldr1 (.>>)

loop_forever :: Seq -> Seq
loop_forever (Seq initial code) = Seq initial $ \self cont -> code self (initial :| _PC := Lit self)

loop_from_to
  :: Ident -- ^ loop counter
  -> AExpr -- ^ from
  -> AExpr -- ^ to
  -> Seq -- ^ loop body
  -> Seq
loop_from_to reg from to (Seq bodyInit body) = Seq
  { seqInit = bodyInit :| reg := from
  , seqCode = \self cont -> body self
       (If (Var reg :=: to)
         cont
         (reg := Var reg + 1 :| _PC := Lit self)
       )
  }

assigns :: Stmt -> Seq
assigns ass = Seq Nop $ \self cont -> [ass :| cont]

-- Compilation

instantiate :: Seq -> (Stmt, PC, [(PC, Stmt)])
instantiate (Seq initial code) =
  let start_pc = 0
  in (initial, start_pc, zip [start_pc..] $ code start_pc (initial :| _PC := Lit start_pc))


compileSwitch states = Case (Var "PC") $ map (\(pc, stmt) -> (Lit pc, stmt)) states

compile name portDecls varDecls (initial_assigns, initial_pc, states) = unlines
    [ "library ieee;"
    , "use ieee.std_logic_1164.all;"
    , "use ieee.numeric_std.all;"
    , ""
    , "entity " ++ name ++ " is"
    , "  port ("
    , "  clk: in std_logic;"
    , unlines portDecls
    , "  );"
    , "end entity;"
    , "architecture " ++ name ++ "_arch of " ++ name ++ " is"
    , "  variable PC : integer range " ++ show initial_pc ++ " to " ++ show (initial_pc + length states - 1) ++
        " := " ++ show initial_pc ++ ";"
    , unlines varDecls
    , "begin"
    , "process"
    , " if rising_edge(clk) then"
    , ppStmt $ compileSwitch states
    , " end if;"
    , "end process;"
    , "end architecture;"
    ]
