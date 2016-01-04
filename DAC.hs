module DAC where

import Expr
import Stmt
import FSM

output (ldac_n, rst_n, sync_n, sda) =
    "ldac_n" := ldac_n
 :| "rst_n" := rst_n
 :| "sync_n" := sync_n
 :| "sda" := sda

dac = seqs
  [ output (0,0,0,0)
  , loop_forever $ seqs
    [ loop_from_to 0 3 $ \index -> output ("cmd" :! index)
    , loop_from_to 0 11 $ \index -> output ("input" :! index)
    , output _
    ]
  ]
