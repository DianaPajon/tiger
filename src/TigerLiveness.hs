module TigerLiveness where

import TigerEmit
import TigerTemp
import Data.Set as S
import Grafo
import Data.Text

--Tipo que modelan el grafo de flujo.
data FlowNode = Node {
    def :: Set Temp
   ,use :: Set Temp
   ,move :: Bool
   ,livein :: Set Temp
   ,liveout :: Set Temp
 }

type FlowGraph = Grafo FlowNode

--No deberíamos tener errores de pattern matching tan adentro del compilador. Pero por las dudas
--Esta función sirve para encontrar labels dentro de un "programa"
findLabel :: [Assem] -> Label -> Assem
findLabel (ins@(Lab{lassem=_, label= l}):as) lab = if l == lab then ins else findLabel as lab
findLabel (x:xs) lab = findLabel xs lab
findLabel [] l =  error $ "Error de pattern matching, label " ++ unpack l  ++ "no encontrada"

defs :: Assem -> Set Temp
defs (Oper {oassem = _, osrc = _, odest = dest, ojump = _}) = S.fromList dest
defs (Mov {massem = _, msrc = _, mdest = dest}) = S.fromList [dest]

uses :: Assem -> Set Temp
uses (Oper {oassem = _, osrc = src, odest = _, ojump = _}) = S.fromList src
uses (Mov {massem = _, msrc = src, mdest = _}) = S.fromList [src]

