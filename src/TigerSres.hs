module TigerSres where

import TigerTips
import TigerTemp
import TigerUnique
import TigerTrans
import TigerFrame



--type FunEntry = (Unique, Label, [Tipo], Tipo, Externa) -- Entrega 1
type FunEntry = (Level, Label, [Tipo], Tipo, Externa) --Entrega 2

--type ValEntry = Tipo --Entrega 1
type ValEntry =  (Tipo, Access, Int) --Entrega 2

data EnvEntry =
    Var ValEntry | Func FunEntry
    deriving Show
