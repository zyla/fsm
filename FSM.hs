{-# LANGUAGE DataKinds, TypeOperators, GADTs #-}
module FSM where

import Prelude hiding (repeat)

import GHC.TypeLits

data Vec n a where
  Nil :: Vec 0 a
  (:>) :: a -> Vec n a -> Vec (n + 1) a

data Signal a

cmd :: Vec 4 Bit
cmd = 0 :> 0 :> 1 :> 1 :> Nil

data Bit = B0 | B1

instance Num Bit where
  fromInteger 0 = B0
  fromInteger 1 = B1

data Seq

output = undefined
repeat = undefined
sample = undefined

dac :: Signal (Vec 12 Bit) -> Seq
dac input = do
  repeat 4 $ \index -> output (0,1,0,cmd !! index)
  repeat 12 $ \index -> do
    val <- sample input
    output (0,1,0,val !! index)
  output (0,1,1,0)
