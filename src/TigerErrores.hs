module TigerErrores where

import           TigerSymbol

-- | La semántica de Demon es que espera que después de 'derror' la computación
-- __no continúe__.
-- 'derror sym >>= _ = derror sym'
class Demon w where
    -- Métodos básicos.
    derror :: Symbol -> w a
    adder :: w a -> Symbol -> w a
    -- Funciones adicionales
    internal :: Symbol -> w a
    internal = derror . addStr "Internal: "
    notfound :: Symbol -> w a
    notfound  = derror . addStr "Not found:"
