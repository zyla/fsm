{-# LANGUAGE DeriveFunctor #-}
import Control.Monad (ap)

type PC = Int

data Seq a = Seq { unSeq :: PC -> (a, PC) } deriving (Functor)

instance Applicative Seq where
  pure = return
  (<*>) = ap

instance Monad Seq where
  return x = Seq $ \pc -> (x, pc)
  Seq m >>= k = Seq $ \pc ->
    let (x, pc') = m pc
    in unSeq (k x) pc'


nextState :: Seq PC
nextState = Seq $ \pc -> (pc, pc + 1)
