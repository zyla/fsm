{-# LANGUAGE OverloadedStrings #-}
module DAC where

import Expr
import Stmt
import FSM

dac_port =
  [ "dac_scl: out std_logic;"
  , "dac_sda: out std_logic;"
  , "dac_ldac_n: out std_logic;"
  , "dac_rst_n: out std_logic;"
  , "dac_sync_n: out std_logic;"

  , "input: in std_logic_vector(11 downto 0)"
  ]

dac_vars =
  [ "signal x: integer range 0 to 11;"
  , "constant cmd : std_logic_vector(3 downto 0) := \"0011\";"
  , "signal sda : std_logic;"
  ]

dac_assigns =
  [ "dac_scl <= clk;"
  , "dac_sda <= sda;"
  ]

output :: (Bit, Bit, Bit, AExpr) -> Seq
output (ldac_n, rst_n, sync_n, sda) = assigns $
    "ldac_n" := LogicLit ldac_n
 :| "rst_n" := LogicLit rst_n
 :| "sync_n" := LogicLit sync_n
 :| "sda" := sda

dac = seqs
  [ output (B0,B0,B0,LogicLit B0)
  , loop_forever $ seqs
    [ loop_from_to "x" 0 3 $ output (B0,B1,B0,"cmd" :! "x")
    , loop_from_to "x" 0 11 $ output (B0,B1,B0,"input" :! "x")
    , output (B0,B1,B1,LogicLit B0)
    ]
  ]

main =
  let vhdl = compile "dac" dac_port dac_vars dac_assigns $ instantiate dac
  in writeFile "/tmp/dac.vhdl" vhdl
