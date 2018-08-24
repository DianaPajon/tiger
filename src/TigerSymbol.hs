module TigerSymbol (Symbol, pack, unpack, addStr, append, appends, T.length, T.empty) where

import           Prelude   as P

import           Data.Text as T

-- | Symbol es como representamos las cadenas a lo largo del compilador...
type Symbol = T.Text

addStr :: String -> Symbol -> Symbol
addStr str = pack . (++) str . unpack

appends :: [Symbol] -> Symbol
appends = P.foldr1 append
