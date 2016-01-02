{-# LANGUAGE DataKinds, TypeOperators, GADTs, DeriveFunctor #-}
module FSM where

import Control.Monad (ap)
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

  (+) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined
  negate = undefined

cmd :: Vec 4 Bit
cmd = 0 :> 0 :> 1 :> 1 :> Nil

bits :: Signal Int -> Signal (Vec n Bit)
bits = undefined
unbits :: Signal (Vec n Bit) -> Signal Int
unbits = undefined

data Seq' a deriving (Functor)
type Seq = Seq' ()

instance Applicative Seq' where
  pure = return
  (<*>) = ap

instance Monad Seq' where
  return = undefined
  (>>=) = undefined

output = undefined

withRegister = undefined

immediate :: a -> Signal a
immediate = undefined

v = immediate

(!!) :: Signal (Vec n a) -> Signal Int -> Signal a
(!!) = undefined

repeat :: Int -> (Signal Int -> Seq) -> Seq
repeat n act = withRegister (v 0) $ \x ->
  fix $ \loop -> do
  eq <- x ==. v n
  if eq
    then return ()
    else do act; loop

dac :: Signal (Vec 12 Bit) -> Seq
dac input = do
  repeat 4 $ \index -> output (0,1,0,immediate cmd !! index)
  repeat 12 $ \index -> output (0,1,0,input !! index)
  output (0,1,1,0)

sineVals :: Vec 100 Int
sineVals = 2048 :> 2176 :> 2304 :> 2431 :> 2557 :> 2680 :> 2801 :> 2919 :> 3034 :> 3145
        :> 3251 :> 3353 :> 3449 :> 3540 :> 3625 :> 3704 :> 3776 :> 3842 :> 3900 :> 3951
        :> 3995 :> 4031 :> 4059 :> 4079 :> 4091 :> 4095 :> 4091 :> 4079 :> 4059 :> 4031
        :> 3995 :> 3951 :> 3900 :> 3842 :> 3776 :> 3704 :> 3625 :> 3540 :> 3449 :> 3353
        :> 3251 :> 3145 :> 3034 :> 2919 :> 2801 :> 2680 :> 2557 :> 2431 :> 2304 :> 2176
        :> 2048 :> 1919 :> 1791 :> 1664 :> 1538 :> 1415 :> 1294 :> 1176 :> 1061 :>  950
        :>  844 :>  742 :>  646 :>  555 :>  470 :>  391 :>  319 :>  253 :>  195 :>  144
        :>  100 :>   64 :>   36 :>   16 :>    4 :>    0 :>    4 :>   16 :>   36 :>   64
        :>  100 :>  144 :>  195 :>  253 :>  319 :>  391 :>  470 :>  555 :>  646 :>  742
        :>  844 :>  950 :> 1061 :> 1176 :> 1294 :> 1415 :> 1538 :> 1664 :> 1791 :> 1919
        :> Nil

sineGen :: Seq
sineGen = repeat 100 $ \index -> dac $ bits $ immediate sineVals !! index

(#.) = 1
