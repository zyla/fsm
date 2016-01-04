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

dac = seqs
  [ output (0,0,0,0)
  , loop_forever $ seqs
    [ loop_from_to "x" 0 3 $ output (0,1,0,"cmd" :! "x")
    , loop_from_to "x" 0 11 $ output (0,1,0,"input" :! "x")
    , output (0,1,1,0)
    ]
  ]
