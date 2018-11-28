module Grafo where

import Data.Set as S
--Ponele que sea útil la clase
import Prelude as P

data Grafo a where
    Grafo :: (Eq a, Show a, Ord a) => S.Set a -> S.Set (a,a) -> Grafo a

--instance Show (Grafo a) where
--    show (Grafo vertices) = show vertices

aristas :: Grafo a ->  Set (a,a)
aristas (Grafo vert aristas) = aristas

succ :: Grafo a -> a -> S.Set a
succ (Grafo vs as) v = 
    S.foldr 
        (\(origen, dest) sucesores -> if origen == v then S.insert dest sucesores else sucesores) 
        S.empty 
        as

pred :: Grafo a -> a -> Set a
pred (Grafo vs as) v = 
    S.foldr 
        (\(origen, dest) predecesores -> if dest == v then S.insert origen predecesores else predecesores) 
        S.empty 
        as

vertices ::  Grafo a -> S.Set a
vertices (Grafo vs as) = vs

agregarVertice :: Grafo a -> a -> Grafo a
agregarVertice (Grafo vs as) v = Grafo (insert v vs) as

agregarVertices :: Grafo a -> [a] -> Grafo a
agregarVertices  grafo vs = P.foldl agregarVertice  grafo vs

eliminarVertice :: Grafo a -> a -> Grafo a
eliminarVertice (Grafo vs as) v =
    Grafo
        (S.filter (/= v) vs)
        (S.filter (\(o,d) -> v /= o && v /= d) as)

eliminarVertice' :: Grafo a -> a -> (Grafo a,S.Set (a,a))
eliminarVertice' (Grafo vs as) v =
    (Grafo
        (S.filter (/= v) vs)
        (S.filter (\(o,d) -> v /= o && v /= d) as),
        S.filter (\(o,d) -> v == o || v == d) as
    )
    
agregarArista :: Grafo a -> (a,a) -> Grafo a
agregarArista (Grafo vs as) (x,y) =
    Grafo
        (S.insert x (S.insert y vs))
        (S.insert (x,y) as)

agregarAristas ::  Grafo a -> [(a,a)] -> Grafo a
agregarAristas grafo as = P.foldl agregarArista  grafo as

agregarArco :: Grafo a -> (a,a) -> Grafo a
agregarArco grafo (x,y) = agregarArista (agregarArista grafo (x,y)) (y,x)

agregarArcos :: Grafo a -> [(a,a)] -> Grafo a
agregarArcos grafo aristas = P.foldl agregarArco grafo aristas

grafoVacio :: (Eq a, Show a, Ord a) =>  Grafo a
grafoVacio = Grafo S.empty S.empty

aristasIncidentes :: Grafo a -> a -> Set (a,a)
aristasIncidentes (Grafo vertices aristas) vertice = S.filter (\(a,b) -> a == vertice || b == vertice) aristas