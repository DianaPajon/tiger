module TigerErrores where

import           TigerSymbol

class Daemon w where
    derror :: Symbol -> w a
    adder :: w a -> Symbol -> w a
    -- adder w s = handle w (derror . append s)
    internal :: Symbol -> w a
    internal = derror . addStr "Internal: "
    notfound :: Symbol -> w a
    notfound  = derror . addStr "Not found:"
