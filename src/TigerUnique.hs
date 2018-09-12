{-# Language MultiParamTypeClasses #-}
module TigerUnique where

import Control.Monad.Trans
import qualified Control.Monad.State.Class as MSt

import Control.Arrow

type Unique = Integer

class UniqueGenerator w where
  mkUnique :: w Unique

-- type StGen = State Integer -- Con un entero nos alcanza

newtype State s v = St {runSt :: s -> (v , s)}

instance Functor (State s) where
  fmap f m = St $ \s' -> first f (runSt m s')

instance Applicative (State s) where
  pure a = St $ \s -> (a, s)
  f <*> x = St $ \s ->
                   let (f' , s' ) = runSt f s
                       (x' , s'' ) = runSt x s'
                   in (f' x' , s'' )

instance Monad (State s) where
  return = pure
  x >>= g = St $ \s ->
                   let (x' , s' ) = runSt x s
                        in runSt (g x') s'

internalGet :: State a a
internalGet = St $ \s -> (s,s)

internalPut :: b -> State b ()
internalPut b = St $ const ((), b)

modify :: (s -> s) -> State s ()
modify f = internalGet >>= (internalPut . f)

internalEvalState :: State s a -> s -> (a, s)
internalEvalState = runSt

instance MSt.MonadState s (State s) where
  get = internalGet
  put = internalPut

type StGen = State Integer
instance UniqueGenerator (State Integer) where
  mkUnique = modify (+1) >> internalGet

instance (Monad m , MonadTrans t , UniqueGenerator m)
         => UniqueGenerator (t m) where
  mkUnique = lift mkUnique
