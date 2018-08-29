module TigerSres where

import TigerTips
import TigerTemp
-- import TigerTrans
-- import TigerFrame

type FunEntry = (Unique, Label, [Tipo], Tipo, Bool)
-- type FunEntry = (Level, Label, [Tipo], Tipo, Bool)
type ValEntry = Tipo -- Entrega2 -> = (Tipo, Access, Int)

data EnvEntry =
    Var ValEntry | Func FunEntry
    deriving Show
