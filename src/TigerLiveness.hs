module TigerLiveness  where

import TigerTemp
import TigerSymbol
import TigerAssem
import Data.Set as S
import Data.Text as T
import Prelude as P
import Grafo as G
import Data.Text

--Tipo que modelan el grafo de flujo.
data FlowNode a = Node {
   dato :: a
  ,ord :: Integer --Simplifica la implementacion de set.
  ,flabel :: Symbol --Simplifica la busqueda de jumps
  ,def :: Set Temp
  ,use :: Set Temp
  ,move :: Bool
} deriving (Eq)

--Para simplificar debugging. 
instance Show (FlowNode a )where
  show f = ""


instance (Eq a)  => Ord (FlowNode a) where
  n1 `compare` n2 = (ord n1) `compare` (ord n2)

type FlowGraph a = Grafo (FlowNode a)

defs :: Assem -> Set Temp
defs (Oper {oassem = _, osrc = _, odest = dest, ojump = _}) = S.fromList dest
defs (Mov {massem = _, msrc = _, mdest = dest}) = S.fromList [dest]
defs (Lab _ _) = S.empty

uses :: Assem -> Set Temp
uses (Oper {oassem = _, osrc = src, odest = _, ojump = _}) = S.fromList src
uses (Mov {massem = _, msrc = src, mdest = _}) = S.fromList [src]
uses (Lab _ _) = S.empty

mkFlowNode :: Assem -> Integer -> FlowNode Assem
mkFlowNode ins i = Node {
  dato = ins
 ,ord = i
 ,flabel = case ins of
           Lab a l -> l
           _ -> T.pack ""
 ,def = defs ins
 ,use = uses ins
 ,move = case ins of
           Mov _ _ _ -> True
           _ -> False
}


nodosBase :: [Assem] -> [FlowNode Assem]
nodosBase lista = P.map (\(a,n) -> mkFlowNode a n) (P.zip lista [0..])

agregarAristasBase :: Grafo (FlowNode Assem) -> [FlowNode Assem] -> FlowGraph Assem
agregarAristasBase grafo [] = grafo
agregarAristasBase grafo [n] = grafo
agregarAristasBase grafo (n1:n2:ns)  = agregarArista (agregarAristasBase grafo (n2:ns)) (n1,n2)

jumps :: FlowNode Assem -> [Label]
jumps nodo = case dato nodo of
  (Oper _ _ _ (Just l)) -> l
  _ -> []

getNodosEtiquetados :: [Label] -> [FlowNode Assem] -> [FlowNode Assem]
getNodosEtiquetados labels nodos = P.filter (\n -> elem (flabel n) labels ) nodos

agregarAristasJumps :: Grafo (FlowNode Assem) -> [FlowNode Assem] -> [FlowNode Assem] -> Grafo (FlowNode Assem)
agregarAristasJumps grafo anteriores [] = grafo
agregarAristasJumps grafo anteriores (n:ns) = 
  case (jumps n) of
    [] -> agregarAristasJumps grafo (anteriores ++ [n]) ns
    ls -> agregarAristasJumps (P.foldr (\a grafo -> agregarArista grafo a) grafo saltos) (anteriores ++ [n]) ns
            where saltos = P.zip (repeat n) (getNodosEtiquetados ls ns)

grafoFlow :: [Assem] -> FlowGraph Assem
grafoFlow lista = 
  let nodos = nodosBase lista
  in agregarAristasJumps (agregarAristasBase (agregarVertices grafoVacio nodos) nodos) [] nodos

data LivenessInfo a = Info {
  nodo :: FlowNode a,
  liveIn :: Set Temp,
  liveOut :: Set Temp
} deriving (Eq, Show)
  

startLiveness :: [FlowNode Assem] -> [LivenessInfo Assem]
startLiveness [] = []
startLiveness (n:ns) = (Info n S.empty S.empty) : startLiveness ns


--Iteracion del algoritmo de punto fijo
iteracionLiveness :: (FlowGraph Assem, [LivenessInfo Assem]) -> (FlowGraph Assem, [LivenessInfo Assem])
iteracionLiveness (grafo,infos) = (grafo,) $
  P.map 
    (\info -> info{
      liveIn = S.union (use (nodo info)) (S.difference (liveOut info) (def (nodo info))),
      liveOut = unions (elems $ S.map (\nodo -> ins nodo) (G.succ grafo (nodo info)))
    })
    infos
 where ins n = liveIn (getNodo n infos)
       getNodo n (m:ms) = if ( n == nodo m) then m else getNodo n ms

--Algoritmo de punto fijo propiamente dicho.
buscarPuntoFijo :: FlowGraph Assem -> [LivenessInfo Assem] -> [LivenessInfo Assem] 
buscarPuntoFijo grafo info = 
  if (info == info'  && (aristas grafo /= S.empty))
    then info
    else buscarPuntoFijo grafo info'
 where (grafo', info') = iteracionLiveness (grafo, info)

--Calculo de livenewss
liveness :: [Assem] -> [LivenessInfo ()]
liveness assems =
  let 
    nodos = nodosBase assems
    puntoFijo = buscarPuntoFijo (grafoFlow  assems) (startLiveness nodos)
  in P.map (\li -> li{nodo = (nodo li){dato = ()}}) puntoFijo

--DEBUGGING EXTRAS

verEstado :: [LivenessInfo a] -> Integer -> String
verEstado [] n = ""
verEstado (s:ss) n = (show n) ++ ": " ++ "nodo: " ++ show (nodo s) ++ "\nins:" ++ show (liveIn s) ++ "\nouts:" ++ show (liveOut s) ++ "\n" ++ verEstado ss (n+1)
