module TigerUnique where

import Control.Monad.State

type Unique = Integer

class UniqueGenerator w where
  mkUnique :: w Unique

type StGen = State Integer -- Con un entero nos alcanza

instance {-# OVERLAPS #-} UniqueGenerator StGen where
  mkUnique = modify (+1) >> get

instance (Monad m , MonadTrans t , UniqueGenerator m)
         => UniqueGenerator (t m) where
  mkUnique = lift mkUnique

