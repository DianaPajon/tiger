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
import Data.Maybe as MB

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
    ,coalesce :: [(Nodo,Nodo)]
    ,freeze :: [Nodo]
    ,spill :: [Nodo]
    ,unique :: Integer
    ,colores :: Set (Temp, Temp) --Colores asignados.
  }



--Vamos con todo, despues quedarà lo que quede. No parece que vaya a salir "elegante" esto
--Es un algoritmo 100% imperativo lo que estoy implementando con la mónada de estado
--BTW: ¿Para que voy a usar una clase si no tiene ningún tipo de abstracción?.
class (Monad a, TLGenerator a) => Allocator a where
    push :: Nodo -> a ()
    pop :: a Nodo
    deg :: Nodo -> a (Maybe Int)
    neg :: Nodo -> a (Set Nodo)
    precolored :: Nodo -> a Bool
    simplifyNode :: Nodo -> a ()
    coalesceNodes :: Nodo -> Nodo -> a () --Redefine todo el estado, hacer un coalesce.
    freezeNode :: Nodo -> a ()
    spillNode :: Nodo -> a ()
    simplifyWorkList :: a [Nodo]
    updateSimplify :: a ()
    coalesceWorkList :: a [(Nodo,Nodo)]
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
        updateSimplify
        updateCoalesce
        updateFreeze
        updateSpill
    pop = do 
        estado <- get
        let (nodo,interf0,moves0):ss = stack estado
        put estado{interf = interf0, moves = moves0, stack = ss}
        updateSimplify
        updateCoalesce
        updateFreeze
        updateSpill
        return nodo
    deg n = do
        estado <- get
        infinito <- precolored n
        if infinito
            then return Nothing
            else do 
                let grafo = interf estado
                let vecinos = S.union (G.pred grafo n) (G.succ grafo n) 
                return $ Just $ size vecinos
    neg n = do
        estado <- get
        let grafo = interf estado
        let vecinos = S.union (G.pred grafo n) (G.succ grafo n) 
        return  vecinos
    precolored n = do --pre está de mas.
        estado <- get
        let coloresDados = colores estado
        let coloreados = S.map (\(s,t) -> s) coloresDados
        return $  S.intersection n coloreados /= S.empty -- ¿Y si coalesceo al FP, o al SP?
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
        let nuevosMoves = S.filter (\(a,b) -> a /= b) $ S.union movsFiltrados (S.union nuevosMovs1 nuevosMovs2)
        put estado{interf = nuevoInterf, moves = nuevosMoves}
        updateSimplify
        updateCoalesce
        updateFreeze
        updateSpill
    setBase assems frame = do
        estado <- get
        put estado{assembly = assems, frame = frame}
    getBase = do
        estado <- get
        return (assembly estado, frame estado)
    start = do
        estado <- get
        (assems, frame) <- getBase
        let interferencias = liftGrafo (liveness assems)
        let moves = findMoves assems
        put $  AS {
            assembly = assems
            ,frame = frame
            ,stack = []
            ,interf = interferencias
            ,moves = moves
            ,simplify = []
            ,coalesce = []
            ,freeze = []
            ,spill = []
            ,unique = unique estado
            ,colores = coloresBase
        }
        updateSimplify
        updateCoalesce
        updateFreeze
        updateSpill
        return () 
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
    updateSimplify = do 
        --simplify está formada por nodos de grado < k que no están asociados a un move.
        estado <- get
        let grafo = interf estado
        let nodos = vertices grafo
        maybeGrados <-  mapM deg (toList nodos)
        let grados = P.map (\x ->MB.fromMaybe 999999 x)  maybeGrados
        let nodosGrados = P.zip (toList nodos) grados
        let nodosGradoBajo = P.filter (\(n,g) -> g < k) nodosGrados
        let movs = moves estado
        let nodosSinMoves = P.filter (\(n,g) -> S.filter (\(a,b) -> a == n || b == n) movs == S.empty ) nodosGradoBajo
        let worklist =  P.map (\(n,g) -> n) $ L.sortBy (\(_,g1) (_,g2) -> compare g1 g2) nodosSinMoves
        put estado{simplify = worklist}
    updateCoalesce = do 
        --esta formado por nodos que tienen muves entre ellos, no tiene interferencias 
        --y al unir sus vecindarios recultan de grado menor que k
        estado <- get
        let grafo = interf estado
        let movs = moves estado
        let movsSinInterf = toList $ S.filter (\(n1,n2) -> not $ conectados grafo n1 n2)  movs
        let movs1 = P.map (\(n1,n2) -> n1) movsSinInterf
        let movs2 = P.map (\(n1,n2) -> n2) movsSinInterf
        vec1 <- mapM neg movs1
        vec2 <- mapM neg movs2
        let vecinos = P.map (\(v1,v2) -> S.union v1 v2) $ P.zip vec1 vec2
        let grados = P.map (\s -> S.size s) vecinos
        let movsGrados = P.zip movsSinInterf grados
        let movsViables = P.filter (\(m,g) -> g < k) movsGrados
        let coal = P.map (\(m,g) -> m ) $ L.sortBy (\(m1,g1) (m2,g2) -> compare g1 g2) movsViables
        put estado{coalesce = coal}
    updateFreeze = do
        --Son nodos de grado bajo "freezeables" pero a su vez relacionados con moves. Freezear
        --Significa eliminar los moves y hacer simplify.
        estado <- get
        let grafo = interf estado
        let movs = moves estado
        let nodos = vertices grafo
        maybeGrados <-  mapM deg (toList nodos)
        let grados = P.map (\x ->MB.fromMaybe 999999 x)  maybeGrados
        let nodosGrados = P.zip (toList nodos) grados
        let nodosGradoBajo = P.filter (\(n,g) -> g < k) nodosGrados
        let nodosEnMoves = P.filter (\(n,g) -> elem n (S.map (\(x,y)->x) movs ) || elem n (S.map (\(x,y)->y) movs )  ) nodosGradoBajo
        let worklist = P.map (\(n,g) -> n) $ L.sortBy (\(n1,g1) (n2,g2) -> compare g1 g2) nodosEnMoves
        put estado{freeze = worklist}
    updateSpill = do
        --Spill son nodos de grado ALTO, que no pueden ser simplificados ni coalesceado.
        estado <- get
        let grafo = interf estado
        let nodos = vertices grafo
        maybeGrados <-  mapM deg (toList nodos)
        let grados = P.map (\x ->MB.fromMaybe 999999 x)  maybeGrados
        let nodosGrados = P.zip (toList nodos) grados
        let nodosGradoAlto = P.filter (\(n,g) -> g >= k) nodosGrados
        let worklist =  P.map (\(n,g) -> n) $ L.sortBy (\(_,g1) (_,g2) -> compare g2 g1) nodosGradoAlto
        put estado{spill = worklist}
        
        




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
spillTemp (op:ops) fr t = do
    (ops',nuevoFrame) <- spillTemp ops fr t
    return (op:ops',nuevoFrame)


--Auxs: 
--Toma el grafo de interferencia de liveness y lo convierte en uno de registros,
liftGrafo :: Grafo Temp -> Grafo Nodo
liftGrafo (Grafo vertices aristas) = G.Grafo (S.map (\a -> fromList [a]) vertices) (S.map (\(a,b)-> (fromList [a], fromList [b])) aristas)

reemplazar :: (Eq a) => a -> a -> a -> a
reemplazar x y z = if z == x then y else z

findMoves :: [Assem] -> Set (Nodo, Nodo)
findMoves [] = S.empty
findMoves ((Mov assem dest src):as) = S.insert (S.singleton dest,S.singleton src)  (findMoves as)
findMoves (a:as) = findMoves as

coloresBase :: Set (Temp, Temp)
coloresBase = S.fromList $ P.zip todosLosRegistros todosLosRegistros