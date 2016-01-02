{-# LANGUAGE DataKinds, TypeOperators #-}
module FSM where

import GHC.TypeLits

data Vec :: Nat -> * -> * where
  Nil :: Vec 0 a
  (:>) :: a -> Vec n a -> Vec (n + 1) a

cmd = 0 :> 0 :> 1 :> 1 :> Nil

dac :: Signal (Vec 12 Bit) -> Seq
dac input = do
  repeat 4 $ \index -> output (0,1,0,cmd !! index)
  repeat 12 $ \index -> do
    val <- sample input
    output (0,1,0,val !! index)
  output (0,1,1,0)
