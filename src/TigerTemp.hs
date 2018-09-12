{-# Language UndecidableInstances #-}
module TigerTemp where

import           Control.Monad.State

import           TigerSymbol
import           TigerUnique

type Label = Symbol
type Temp  = Symbol

makeStringT :: Temp -> String
makeStringT = unpack

makeStringL :: Label -> String
makeStringL = unpack

detgenTemp :: Integer -> Temp
detgenTemp i = pack ("T"++show i)

detgenLabel :: Integer -> Label
detgenLabel i = pack ("L"++show i)

-- | Clase generadora de temps, y labels
class TLGenerator w where
    newTemp :: w Temp
    newLabel :: w Label

instance (Monad p , UniqueGenerator p) => TLGenerator p where
  newTemp = detgenTemp <$> mkUnique
  newLabel = detgenLabel <$> mkUnique

instance (MonadTrans t, TLGenerator m, Monad m) => TLGenerator (t m) where
  newTemp = lift newTemp
  newLabel = lift newLabel
