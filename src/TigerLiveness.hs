module TigerLiveness where

import TigerEmit
import TigerTemp
import Data.Set as S
import Grafo

defs :: Assem -> Set Temp
defs (Oper {oassem = _, osrc = _, odest = dest, ojump = _}) = S.fromList dest
defs (Mov {massem = _, msrc = _, mdest = dest}) = S.fromList [dest]

uses :: Assem -> Set Temp
uses (Oper {oassem = _, osrc = src, odest = _, ojump = _}) = S.fromList src
uses (Mov {massem = _, msrc = src, mdest = _}) = S.fromList [src]
