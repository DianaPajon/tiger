module TigerTemp where

import           Control.Monad.State

import           TigerSymbol

type Label = Symbol
type Temp  = Symbol

makeStringT :: Temp -> String
makeStringT = unpack

makeStringL :: Label -> String
makeStringL = unpack

detgenTemp :: Int -> Temp
detgenTemp i = pack ("T"++show i)

detgenLabel :: Int -> Label
detgenLabel i = pack ("L"++show i)

-- | Clase generadora de temps, y labels
class TLGenerator w where
    newTemp :: w Temp
    newLabel :: w Label

type StGen = State Int
instance TLGenerator StGen where
  newTemp = modify (+1) >> gets detgenTemp
  newLabel = modify (+1) >> gets detgenLabel

instance MonadTrans t => TLGenerator (t StGen) where
  newTemp = lift newTemp
  newLabel = lift newLabel
