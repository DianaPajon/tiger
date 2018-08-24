{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module TigerTemp where
import           Control.Monad.State
import qualified Data.Text           as T

import TigerSymbol

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
