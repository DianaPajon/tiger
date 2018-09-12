module TigerTips where

import           TigerErrores
import           TigerSymbol
import TigerUnique

data RWO = RW | RO
    deriving (Show,Eq)

-- | Representa la posición en memoria en la que va a estar el campo
-- de un record
-- Es decir, el offset en palabras del puntero a la /primer posición/.
type Posicion = Int

-- | 'Tipo' es la representación interna de los tipos,
-- son los tipos que maneja el compilador. En código
-- fuente 'int' es representado originalmente por 'TInt' de
-- forma interna al compilador.
data Tipo = TUnit
          -- ^ Tipo Unit
          | TString
          -- ^ Tipo String
          | TNil
          -- ^ Tipo Nil. Es un poco especial
          | TInt RWO
          -- ^ Tipo Int, aunque hay dos de estos. Ver 'RWO'
          | TArray Tipo Unique
          -- ^ Tipo de los arreglos (de otro tipo) que necesita de un
          -- identificador /único/.
          | TRecord [(Symbol, Tipo, Posicion)] Unique
          -- ^ Tipo de los records, similar a los arreglos pero con campos
          -- con nombre.
          -- | Referencia a un record y a un Tipo.
          -- Estos elementos son útiles en el proceso de detección de ciclos,
          -- pero no deberían verse una vez realizada la detección.
          | RefRecord Symbol | TTipo Symbol
    deriving (Show,Eq)

-- | Internamente capaz que es mejor usar esta construcción.
-- hacemos abuso de una extensión de Ghc para dar una construcción
-- que se comporta como un constructor más de 'Tipo' pero
-- es un simple sinónimo a 'TInt RW'.
pattern TBool :: Tipo
pattern TBool <- (TInt RW)
  where TBool = TInt RW

-- TODO: Para los estudiantes avanzados, estaría bueno hacer otro tipo que
-- fuerce dicha noción. Lo fácil es hacer otro tipo que nos los tenga, y
-- hacer las funciones correspondientes para tener una 'TipoConRef -> TipoSinRef'.

-- | Hay ciertos tipos que semánticamente son equivalentes, implementamos
-- una función que nos indica cuando son equivalentes.
equivTipo :: Tipo -> Tipo -> Bool -- optimizar?
equivTipo TRecord{} TNil                = True
equivTipo TNil TRecord{}                = True
equivTipo (TRecord _ u1) (TRecord _ u2) = u1 == u2
equivTipo (TArray _ u1) (TArray _ u2)   = u1 == u2
equivTipo (TInt _) (TInt _)             = True
-- TTipo es una referencia a otro tipo, no deberíamos
-- encontrarnos ninguno de estos tipos.
equivTipo (TTipo _) _                   = error "Referencia a un tipo.[1]"
equivTipo _ (TTipo _)                   = error "Referencia a un tipo.[2]"
equivTipo a b                           = a == b -- Eq

-- | Función /linda/ para mostrar un error de tipos.
errorTipos :: Demon w => Tipo -> Tipo -> w a
errorTipos t1 t2 = derror $ pack $ "Error de tipos."
                   -- Notar que acá se van a mostrar de forma re crota.
                   ++ " Tipo *" ++ show t1
                   ++ "* es distinto a *"
                   ++ show t2 ++ "*."
