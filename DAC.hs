{-# LANGUAGE OverloadedStrings #-}
module DAC where

import Expr
import Stmt
import FSM

output :: (AExpr, AExpr, AExpr, AExpr) -> Seq
output (ldac_n, rst_n, sync_n, sda) = assigns $
    "ldac_n" := ldac_n
 :| "rst_n" := rst_n
 :| "sync_n" := sync_n
 :| "sda" := sda

dac_port =
  [ "clk: in std_logic;"
  , "dac_scl: out std_logic;"
  , "dac_sda: out std_logic;"
  , "dac_ldac_n: out std_logic;"
  , "dac_rst_n: out std_logic;"
  , "dac_sync_n: out std_logic;"

  , "input: in std_logic_vector(11 downto 0)"
  ]

dac = seqs
  [ output (0,0,0,0)
  , loop_forever $ seqs
    [ loop_from_to "x" 0 3 $ output (0,1,0,"cmd" :! "x")
    , loop_from_to "x" 0 11 $ output (0,1,0,"input" :! "x")
    , output (0,1,1,0)
    ]
  ]
