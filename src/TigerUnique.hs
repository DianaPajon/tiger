{-# Language MultiParamTypeClasses #-}
module TigerUnique where

import Control.Monad.Trans
import Control.Monad.State.Class hiding (modify)

import State as St

-- | Los 'Uniques' nos vendrán bien para definir números que supondremos únicos.
-- Ya que los vamos a usar a lo largo del compilador, voy a definir una mónada
-- que los va a manejar.
-- Es una mónada de estados, pero no vamos a usar la definida por la librería
-- ya que eso nos traería problemas con la instancia de Transformer definida
-- más abajo. Porque 'State a = StateT Identity a'.
-- Es más para que no se use mucho este 'State' lo separo en otro archivo.

evalState :: State s a -> s -> (a , s)
evalState = runSt

-- | Claramente los 'Unique' propiamente dichos son enteros.
type Unique = Integer

-- | Usamos una Clase para definir algo que nos genera estos enteros.
class UniqueGenerator w where
  mkUnique :: w Unique

-- | Damos una instancia de 'State' para que pueda ser usada como una mónada
-- state más...
instance MonadState s (State s) where
  state = St

type StGen = State Integer

-- | Damos la instancia inicial de que es un generador de Uniques.
instance UniqueGenerator StGen where
  mkUnique = modify (+1) >> St.get

-- | Y acá viene la magia... Ya que vamos a usar monad transformers.
-- Doy una instancia general que los recorre hasta llegar a un generador de uniques,
-- y mete todos los lifts necesarios...
-- Ejemplo, suponete que usamos 3 monad transformers.
-- MT1 (MT2 (MT3 StGen)) a
-- para generar uniques tendríamos que llamar a (lift (lift (lift mkUnique)))
-- pero como nos da pereza hacer eso, hacemos que lo haga Haskell por nosotros.
-- y así queda la onda:
instance (Monad m , MonadTrans t , UniqueGenerator m)
         => UniqueGenerator (t m) where
  mkUnique = lift mkUnique
