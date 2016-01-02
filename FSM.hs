{-# LANGUAGE DataKinds, TypeOperators, GADTs, DeriveFunctor #-}
module FSM where

import Prelude hiding (repeat, (!!))

import GHC.TypeLits

data Vec n a where
  Nil :: Vec 0 a
  (:>) :: a -> Vec n a -> Vec (n + 1) a

infixr 5 :>

data Signal a

data Bit = B0 | B1

instance Num Bit where
  fromInteger 0 = B0
  fromInteger 1 = B1

cmd :: Signal (Vec 4 Bit)
cmd = immediate (0 :> 0 :> 1 :> 1 :> Nil)

data Seq' a deriving (Functor)
type Seq = Seq' ()

instance Applicative Seq'

instance Monad Seq' where
  return = undefined
  (>>=) = undefined

output = undefined
sample = undefined

immediate :: a -> Signal a
immediate = undefined

(!!) :: Signal (Vec n a) -> Signal Int -> Signal a
(!!) = undefined

repeat :: Int -> (Signal Int -> Seq) -> Seq
repeat = undefined

dac :: Signal (Vec 12 Bit) -> Seq
dac input = do
  repeat 4 $ \index -> output (0,1,0,cmd !! index)
  repeat 12 $ \index -> do
    val <- sample input
    output (0,1,0,val !! index)
  output (0,1,1,0)
