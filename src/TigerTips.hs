module TigerTips where

import           TigerSymbol

type Unique = Int

data RWO = RW | RO
    deriving (Show,Eq)

data Tipo = TUnit | TString
          | TNil | TInt RWO
          | TArray Tipo Unique
          | TRecord [(Symbol, Tipo, Int)] Unique
          -- | Tipos que no deberían volver a verse.
          -- Tengo que emprolijar esto...
          | RefRecord Symbol | TTipo Symbol
    deriving (Show,Eq)

intiposIguales :: Tipo -> Tipo -> Bool -- optimizar?
intiposIguales (TRecord {}) TNil             = True
intiposIguales TNil (TRecord {})             = True
intiposIguales (TRecord _ u1) (TRecord _ u2) = u1 == u2
intiposIguales (TArray _ u1) (TArray _ u2)   = u1 == u2
intiposIguales (TInt _) (TInt _)             = True
intiposIguales (TTipo _) _                   =error "TTIPOOOOOOOOOO 1"
intiposIguales _ (TTipo _)                   =error "TTIPOOOOOOOOOO 2"
intiposIguales a b                           = a == b -- Eq

