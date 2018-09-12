{-# Language MultiParamTypeClasses #-}
module TigerUnique where

import Control.Monad.Trans
import Control.Monad.State.Class hiding (modify)

import State

evalState :: State s a -> s -> (a , s)
evalState = runSt

type Unique = Integer

class UniqueGenerator w where
  mkUnique :: w Unique

-- type StGen = State Integer -- Con un entero nos alcanza

instance MonadState s (State s) where
  state = St

type StGen = State Integer
instance UniqueGenerator (State Integer) where
  mkUnique = modify (+1) >> internalGet

instance (Monad m , MonadTrans t , UniqueGenerator m)
         => UniqueGenerator (t m) where
  mkUnique = lift mkUnique
