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
  , "signal sda : std_logic"
  ]

dac_assigns =
  [ "dac_scl <= clk;"
  , "dac_sda <= sda;"
  ]

output :: (AExpr, AExpr, AExpr, AExpr) -> Seq
output (ldac_n, rst_n, sync_n, sda) = assigns $
    "ldac_n" := ldac_n
 :| "rst_n" := rst_n
 :| "sync_n" := sync_n
 :| "sda" := sda

dac = seqs
  [ output (0,0,0,0)
  , loop_forever $ seqs
    [ loop_from_to "x" 0 3 $ output (0,1,0,"cmd" :! "x")
    , loop_from_to "x" 0 11 $ output (0,1,0,"input" :! "x")
    , output (0,1,1,0)
    ]
  ]

main =
  let vhdl = compile "dac" dac_port dac_vars dac_assigns $ instantiate dac
  in writeFile "/tmp/dac.vhdl" vhdl
