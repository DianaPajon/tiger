module TigerAllocation where
import TigerAssem
import Grafo as G
import TigerLiveness
import TigerShow 
import TigerFrame
import TigerTemp
import Data.Set as S
import Prelude as P
import Data.Text as T
import Data.List as L

import Control.Monad.State

type Nodo = Set Temp --Uso esta generalización para poder representar nodos "coalesceados"

data AllocState = 
 AS {
     assembly :: [Assem]
    ,frame :: Frame
    ,stack :: [(Nodo, Grafo Nodo, Set (Nodo, Nodo))] --Stack del grafo de liveness.
    ,interf :: Grafo Nodo
    ,moves :: Set (Nodo, Nodo)
    ,simplify :: [Nodo]
    ,coalesce :: [Nodo]
    ,freeze :: [Nodo]
    ,spill :: [Nodo]
    ,unique :: Integer
    ,colores :: [(Temp, Temp)] --Colores asignados.
  }



--Vamos con todo, despues quedarà lo que quede. No parece que vaya a salir "elegante" esto
--Es un algoritmo 100% imperativo lo que estoy implementando con la mónada de estado
class (Monad a, TLGenerator a) => Allocator a where
    push :: Nodo -> a ()
    pop :: a Nodo
    deg :: Nodo -> a Int
    neg :: Nodo -> a [Nodo]
    precolored :: Nodo -> a Bool
    coalesceNodes :: Nodo -> Nodo -> a () --Redefine todo el estado, hacer un coalesce.
    simplifyWorkList :: a [Nodo]
    updateSimplfy :: a ()
    coalesceWorkList :: a [Nodo]
    updateCoalesce :: a ()
    freezeWorkList :: a [Nodo]
    updateFreeze :: a ()
    spillWorkList :: a [Nodo]
    updateSpill :: a ()
    setBase :: [Assem] -> Frame -> a ()
    getBase :: a ([Assem], Frame)
    start :: a ()

type Alloc  = State AllocState

instance TLGenerator Alloc where
    newTemp = do estado <- get
                 let u = unique estado
                 put estado{unique = u + 1}
                 return $ T.pack ("t" ++ show u)
    newLabel = do estado <- get
                  let u = unique estado
                  put estado{unique = u + 1}
                  return $ T.pack ("L" ++ show u)
 
instance Allocator Alloc where
    push n = do
        estado <- get
        let nuevoStack = (n, interf estado, moves estado):(stack estado)
        let nuevoGrafo = eliminarVertice (interf estado) n
        let nuevosMoves = S.filter (\(t1,t2) -> (S.intersection t1 n == S.empty) && (S.intersection t1 n == S.empty)) $ moves estado
        put estado{interf = nuevoGrafo, stack = nuevoStack, moves = nuevosMoves}
        updateSimplfy
        updateCoalesce
        updateFreeze
        updateSpill
    pop = do 
        estado <- get
        let (nodo,interf0,moves0):ss = stack estado
        put estado{interf = interf0, moves = moves0, stack = ss}
        updateSimplfy
        updateCoalesce
        updateFreeze
        updateSpill
        return nodo
    deg n = do
        estado <- get
        let grafo = interf estado
        let vecinos = S.union (G.pred grafo n) (G.succ grafo n) 
        return $ size vecinos
    neg n = do
        estado <- get
        let grafo = interf estado
        let vecinos = S.union (G.pred grafo n) (G.succ grafo n) 
        return $ toList vecinos
    precolored n = return $  L.intersect (toList n) (registrosGenerales ++ specialregs) /= [] -- ¿Y si coalesceo al FP, o al SP?
    coalesceNodes n1 n2 = do
        estado <- get
        let grafo = interf estado --Código ..."imperativo"
        let incidentes1 = aristasIncidentes grafo n1
        let incidentes2 = aristasIncidentes grafo n2
        let nuevoNodo = S.union n1 n2
        let nuevasIncidentes1 = S.map (\(a,b) -> (reemplazar n1 nuevoNodo a , reemplazar n1 nuevoNodo b)) incidentes1
        let nuevasIncidentes2 = S.map (\(a,b) -> (reemplazar n2 nuevoNodo a , reemplazar n2 nuevoNodo b)) incidentes2
        let nuevasIncidentes = S.union nuevasIncidentes1 nuevasIncidentes2
        let grafoBase = eliminarVertice (eliminarVertice grafo n1) n2
        let nuevoInterf = agregarAristas (agregarVertice grafoBase nuevoNodo) (toList nuevasIncidentes)
        let movs1 = S.filter (\(a,b) -> a == n1 || b == n1) $ moves estado
        let movs2 = S.filter (\(a,b) -> a == n2 || b == n2) $ moves estado
        let nuevosMovs1 = S.map (\(a,b) -> (reemplazar n1 nuevoNodo a, reemplazar n1 nuevoNodo b)) movs1
        let nuevosMovs2 = S.map (\(a,b) -> (reemplazar n2 nuevoNodo a, reemplazar n2 nuevoNodo b)) movs2
        let movsFiltrados = S.filter (\(a,b) -> a /= n1 && a /= n2 && b /= n1 && b /= n2) $ moves estado
        let nuevosMoves = S.union movsFiltrados (S.union nuevosMovs1 nuevosMovs2)
        put estado{interf = nuevoInterf, moves = nuevosMoves}
        updateSimplfy
        updateCoalesce
        updateFreeze
        updateSpill
    setBase assems frame = do
        estado <- get
        put estado{assembly = assems, frame = frame}
    getBase = do
        estado <- get
        return (assembly estado, frame estado)
    simplifyWorkList = do
        estado <- get
        return $ simplify estado
    coalesceWorkList = do
        estado <- get
        return $ coalesce estado
    freezeWorkList = do
        estado <- get
        return $ freeze estado
    spillWorkList = do
        estado <- get
        return $ spill estado



retrieveTemp t fr = Oper {oassem="movl `d0, [ebp +" ++ show (nextTemp fr) ++ "]",osrc=[],odest=[t],ojump = Nothing }
saveTemp t fr = Oper {oassem="movl [ebp +" ++ show (nextTemp fr) ++ "],`s0",osrc=[t],odest=[],ojump = Nothing }

spillTemp :: (Allocator a) => [Assem] -> Frame -> Temp -> a ([Assem],Frame)
spillTemp [] fr t = return ([],fr{actualReg = actualReg fr +1})
spillTemp (op@(Oper _ dest src  _):ops) fr t = do
    let define = elem t dest
    let usa = elem t src
    te <- newTemp
    (ops',nuevoFrame) <- spillTemp ops fr t
    if(usa || define)
        then let previa = if usa then [retrieveTemp te fr] else []
                 siguiente = if define then [saveTemp te fr] else []
             in return (previa ++ [op] ++ siguiente ++ ops', nuevoFrame)
        else return (op:ops',nuevoFrame)
spillTemp (op@(Mov _ dest src  ):ops) fr t = do
    let define =  t ==  dest
    let usa = t == src
    te <- newTemp
    (ops',nuevoFrame) <- spillTemp ops fr t
    if(usa || define)
        then let previa = if usa then [retrieveTemp te fr] else []
                 siguiente = if define then [saveTemp te fr] else []
             in return (previa ++ [op] ++ siguiente ++ ops', nuevoFrame)
        else return (op:ops',nuevoFrame)
spillNode (op:ops) fr t = do
    (ops',nuevoFrame) <- spillTemp ops fr t
    return (op:ops',nuevoFrame)


--Auxs: 
--Toma el grafo de interferencia de liveness y lo convierte en uno de registros,
liftGrafo :: Grafo Temp -> Grafo Nodo
liftGrafo (Grafo vertices aristas) = G.Grafo (S.map (\a -> fromList [a]) vertices) (S.map (\(a,b)-> (fromList [a], fromList [b])) aristas)

reemplazar :: (Eq a) => a -> a -> a -> a
reemplazar x y z = if z == x then y else z
