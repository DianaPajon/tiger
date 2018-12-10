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
import Data.Map as M

import Control.Monad.State

type Nodo = Set Temp --Uso esta generalización para poder representar nodos "coalesceados"

data AllocState = 
 AS {
     stack :: [(Nodo, Grafo Nodo)] --Stack del grafo de liveness.
    ,interf :: Grafo Nodo
    ,moves :: Set (Nodo, Nodo)
    ,simplify :: [Nodo]
    ,coalesce :: [(Nodo,Nodo)]
    ,freeze :: [Nodo]
    ,spill :: [Nodo]
    ,unique :: Integer
  }



--Vamos con todo, despues quedarà lo que quede. No parece que vaya a salir "elegante" esto
--Es un algoritmo 100% imperativo lo que estoy implementando con la mónada de estado
--BTW: ¿Para que voy a usar una clase si no tiene ningún tipo de abstracción?.
class (Monad a, TLGenerator a) => Allocator a where
    push :: Nodo -> a ()
    deg :: Nodo -> a (Maybe Int)
    neg :: Nodo -> a (Set Nodo)
    precolored :: Nodo -> a Bool
    simplifyNode :: Nodo -> a ()
    coalesceNodes :: (Nodo, Nodo) -> a () --Redefine todo el estado, hacer un coalesce.
    freezeNode :: Nodo -> a ()
    spillNode :: [Assem] -> Frame -> Nodo -> a ([Assem], Frame)
    simplifyWorkList :: a [Nodo]
    updateSimplify :: a ()
    coalesceWorkList :: a [(Nodo,Nodo)]
    updateCoalesce :: a ()
    freezeWorkList :: a [Nodo]
    updateFreeze :: a ()
    spillWorkList :: a [Nodo]
    updateSpill :: a ()
    start :: [Assem] -> Frame -> a ()
    allocMain :: [Assem] -> Frame -> a ([Assem],Frame)

type AllocST  = State AllocState

instance TLGenerator AllocST where
    newTemp = do estado <- get
                 let u = unique estado
                 put estado{unique = u + 1}
                 return $ T.pack ("t" ++ show u)
    newLabel = do estado <- get
                  let u = unique estado
                  put estado{unique = u + 1}
                  return $ T.pack ("L" ++ show u)
 
instance Allocator AllocST where
    push n = do
        estado <- get
        let nuevoStack = (n, interf estado):(stack estado)
        let nuevoGrafo = eliminarVertice (interf estado) n
        let nuevosMoves = S.filter (\(t1,t2) -> (S.intersection t1 n == S.empty) && (S.intersection t1 n == S.empty)) $ moves estado
        put estado{interf = nuevoGrafo, stack = nuevoStack, moves = nuevosMoves}
        updateSimplify
        updateCoalesce
        updateFreeze
        updateSpill
    deg n = do
        estado <- get
        infinito <- precolored n
        if infinito
            then return Nothing
            else do 
                let grafo = interf estado
                let vecinos = S.union (G.pred grafo n) (G.succ grafo n) 
                return $ Just $ S.size vecinos
    neg n = do
        estado <- get
        let grafo = interf estado
        let vecinos = S.union (G.pred grafo n) (G.succ grafo n) 
        return  vecinos
    precolored n = do --pre está de mas.
        estado <- get
        let coloresDados = coloresBase
        let coloreados = S.map (\(s,t) -> s) $ S.fromList $ M.toAscList coloresDados
        return $  S.intersection n coloreados /= S.empty -- ¿Y si coalesceo al FP, o al SP?
    coalesceNodes (n1, n2) = do
        estado <- get
        let grafo = interf estado --Código ..."imperativo"
        let incidentes1 = aristasIncidentes grafo n1
        let incidentes2 = aristasIncidentes grafo n2
        let nuevoNodo = S.union n1 n2
        let nuevasIncidentes1 = S.map (\(a,b) -> (reemplazarL [n1,n2] nuevoNodo a , reemplazarL [n1,n2] nuevoNodo b)) incidentes1
        let nuevasIncidentes2 = S.map (\(a,b) -> (reemplazarL [n1,n2] nuevoNodo a , reemplazarL [n1,n2] nuevoNodo b)) incidentes2
        let nuevasIncidentes = S.filter (\(a,b) -> a /= b) $ S.union nuevasIncidentes1 nuevasIncidentes2
        let grafoBase = eliminarVertice (eliminarVertice grafo n1) n2
        let nuevoInterf = agregarAristas (agregarVertice grafoBase nuevoNodo) (S.toList nuevasIncidentes)
        let movs1 = S.filter (\(a,b) -> a == n1 || b == n1) $ moves estado
        let movs2 = S.filter (\(a,b) -> a == n2 || b == n2) $ moves estado
        let nuevosMovs1 = S.map (\(a,b) -> (reemplazarL [n1,n2] nuevoNodo a, reemplazarL [n1,n2] nuevoNodo b)) movs1
        let nuevosMovs2 = S.map (\(a,b) -> (reemplazarL [n1,n2] nuevoNodo a, reemplazarL [n1,n2] nuevoNodo b)) movs2
        let movsFiltrados = S.filter (\(a,b) -> a /= n1 && a /= n2 && b /= n1 && b /= n2) $ moves estado
        let nuevosMoves = S.filter (\(a,b) -> a /= b) $ S.union movsFiltrados (S.union nuevosMovs1 nuevosMovs2)
        put estado{interf = nuevoInterf, moves = nuevosMoves}
        updateSimplify
        updateCoalesce
        updateFreeze
        updateSpill
        estado <- get
        put estado
    simplifyNode n = do
        --Simplificar un nodo significa eliminarlo del grafo, pushearlo al stack y 
        -- ... nada mas
        estado <- get
        let movs = moves estado
        let grafo = interf estado
        let grafo' = eliminarVertice grafo n
        let movs' = S.filter (\(o,d) -> o /= n && d /= n) movs
        put estado{interf = grafo', moves = movs',stack=(n,grafo):(stack estado)}
        updateSimplify
        updateCoalesce
        updateFreeze
        updateSpill
    freezeNode n = do
        estado <- get
        let movs = moves estado
        let movs' = S.filter (\(o,d) -> o /= n && d /= n) movs
        put estado{moves = movs'}
        updateSimplify
        updateCoalesce
        updateFreeze
        updateSpill
    spillNode assembly frame n = do
        --Para spillear, primero reconstruyo el grafo original, me quedo con el nodo
        --de mayor grado y lo spilleo puntualmente, luego... reconstruyo todo de nuevo
        --PORQUE PUEDO.
        start assembly frame --Tengo un estado original, de nuevo.
        let nodos = P.map (\n -> S.singleton n) $ S.toList n
        grados <- mapM deg nodos
        let nodosGrados = P.zip nodos $ P.map (\n -> fromMaybe 999999 n) grados
        --Como estan siendo elegidos para spill, NINGUNO está precoloreado, porque uno precoloreado precolorea el nodo.
        let nodosOrdenados = L.sortBy (\(_,g1) (_,g2) -> compare g2 g1 ) nodosGrados
        let tempElegido = P.head $ S.toList $ P.head $ P.map (\(a,b) -> a) nodosOrdenados
        estado <- get
        (assems', frame') <- spillTemp assembly frame tempElegido
        return (assems', frame')
    start assems frame = do
        estado <- get
        let interferencias = liftGrafo (liveness assems)
        let moves = findMoves assems
        put $  AS { --Mantiene solamente el unique.
            stack = []
            ,interf = interferencias
            ,moves = moves
            ,simplify = []
            ,coalesce = []
            ,freeze = []
            ,spill = []
            ,unique = unique estado
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
        maybeGrados <-  mapM deg (S.toList nodos)
        let nodosGrados = P.zip (S.toList nodos) maybeGrados
        let nodosGradoBajo = P.filter (\(n,g) -> MB.maybe False (< k) g) nodosGrados
        let movs = moves estado
        let nodosSinMoves = P.filter (\(n,g) -> S.filter (\(a,b) -> a == n || b == n) movs == S.empty ) nodosGradoBajo
        let worklist =  P.map (\(n,g) -> n) $ L.sortBy (\(_,g1) (_,g2) -> compare g1 g2) nodosSinMoves
        put estado{simplify = worklist}
    updateCoalesce = do 
        --esta formado por nodos que tienen moves entre ellos, no tiene interferencias 
        --y al unir sus vecindarios recultan de grado menor que k
        estado <- get
        let grafo = interf estado
        let movs = moves estado
        let movsSinInterf0 = S.toList $ S.filter (\(n1,n2) -> not $ conectados grafo n1 n2)  movs
        movsSinInterf <- filtrarPrecoloreados movsSinInterf0
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
        maybeGrados <-  mapM deg (S.toList nodos)
        let nodosGrados = P.zip (S.toList nodos) maybeGrados
        let nodosGradoBajo = P.filter (\(n,g) -> MB.maybe False (< k) g) nodosGrados
        let nodosEnMoves = P.filter (\(n,g) -> elem n (S.map (\(x,y)->x) movs ) || elem n (S.map (\(x,y)->y) movs )  ) nodosGradoBajo
        let worklist = P.map (\(n,g) -> n) $ L.sortBy (\(n1,g1) (n2,g2) -> compare g1 g2) nodosEnMoves
        put estado{freeze = worklist}
    updateSpill = do
        --Spill son nodos de grado ALTO, que no pueden ser simplificados ni coalesceado.
        estado <- get
        let grafo = interf estado
        let nodos = vertices grafo
        maybeGrados <-  mapM deg (S.toList nodos)
        let nodosGrados = P.zip (S.toList nodos) maybeGrados
        let nodosGradoAlto = P.filter (\(n,g) -> MB.maybe False (>= k) g ) nodosGrados
        let worklist =  P.map (\(n,g) -> n) $ L.sortBy (\(_,g1) (_,g2) -> compare g2 g1) nodosGradoAlto
        put estado{spill = worklist}
    allocMain assembly frame = do
        start assembly frame
        consumirGrafo
        estado <- get
        let pila = stack estado
        let grafoFinal = interf estado
        let coloresIniciales = agregarColores (S.toList $ vertices grafoFinal) coloresBase
        let colores = select coloresIniciales pila
        case colores of 
            Right mapa -> do
                return $ (aplicarColores mapa assembly, frame)
            Left nodo -> do
                (assembly', frame') <- spillNode assembly frame nodo
                allocMain assembly' frame'

agregarColores :: [Nodo] -> Map Temp Temp -> Map Temp Temp
agregarColores [] mapa = mapa
agregarColores (n:ns) mapa = P.foldl (\m (k,v) -> M.insert k v m) (agregarColores ns mapa) (S.map (\t -> (t, MB.maybe t id (colorear n mapa))) n)

consumirGrafo :: (Allocator a) => a ()
consumirGrafo = do
    simWL <- simplifyWorkList
    coalWL <- coalesceWorkList
    frWL <- freezeWorkList
    spiWL <- spillWorkList
    case (simWL, coalWL, frWL, spiWL) of
        (a:as,_,_,_) -> do simplifyNode a
                           consumirGrafo
        ([],c:cs,_,_) -> do coalesceNodes c
                            consumirGrafo
        ([],[],f:fs,_) -> do freezeNode f
                             consumirGrafo
        ([],[],[],s:ss) -> do simplifyNode s 
                              consumirGrafo
        ([],[],[],[]) -> return ()

aplicarColores :: Map Temp Temp -> [Assem] -> [Assem]
aplicarColores mapa [] = []
aplicarColores mapa ((Oper ensamblador destino origen jump):os) = (
    Oper 
    ensamblador
    (P.map (\t -> buscarColor mapa t) destino)
    (P.map (\t -> buscarColor mapa t) origen)
    jump
 ) : (aplicarColores mapa os)
aplicarColores mapa ((Mov ensamblador destino origen ):os) = 
    if colorDestino == colorOrigen
    then aplicarColores mapa os
    else (
        Mov 
        ensamblador
        colorDestino
        colorOrigen
     ):(aplicarColores mapa os)
 where colorDestino = buscarColor mapa destino
       colorOrigen = buscarColor mapa origen
aplicarColores mapa (l:os) = l : aplicarColores mapa os

buscarColor :: Map Temp Temp -> Temp -> Temp
buscarColor mapa t = 
    case M.lookup t ( M.fromList $ M.toAscList mapa)  of --sin esa chanchada, no encuentra lo que esta fuera de orden.
        Just t -> t
        Nothing -> error $ "Color no encontrado: " ++ unpack t ++ show mapa

elegirColor :: Nodo -> Map Temp Temp -> Grafo Nodo -> Maybe Temp
elegirColor nodo mapa grafo = 
    let vecinos = S.union (G.succ grafo nodo) (G.pred grafo nodo)
        coloresVecinos = setMaybes $  S.map (\n -> colorear n mapa) vecinos
        coloresAplicables = S.fromList registrosGenerales
        coloresDisponibles = S.difference coloresAplicables coloresVecinos
    in if (coloresDisponibles == S.empty)
        then Nothing
        else Just $ P.head $ S.toList $ coloresDisponibles

select :: Map Temp Temp ->  [(Nodo, Grafo Nodo)] -> Either Nodo (Map Temp Temp)
select mapa nodos = 
    case nodos of 
        [] -> Right mapa
        ((nodo,grafo):ss) -> 
            case elegirColor nodo mapa grafo of
                Nothing -> Left nodo
                Just t -> case select (insertarColor mapa nodo t) ss of -- ¿Que profundidad espero de esta recursion? ¿Cuanto optimiza haskell esto? No me gusta
                    Left n -> Left n
                    Right m -> Right m



retrieveTemp t fr = Oper {oassem="mov `d0, [%ebp " ++ show (nextTemp fr) ++ "]",osrc=[],odest=[t],ojump = Nothing }

saveTemp t fr = Oper {oassem="mov [%ebp " ++ show (nextTemp fr) ++ "],`s0",osrc=[t],odest=[],ojump = Nothing }

cambiarTemp (Oper assembly dest src jump) tempViejo tempNuevo =
    Oper
    assembly
    (P.map (\x -> reemplazar tempViejo tempNuevo x) dest)
    (P.map (\x -> reemplazar tempViejo tempNuevo x) src)
    jump
cambiarTemp (Mov assembly dest src) tempViejo tempNuevo =
    Mov
    assembly
    (reemplazar tempViejo tempNuevo dest)
    (reemplazar tempViejo tempNuevo src)
cambiarTemp i _ _ = i
--Es monadico eunque no quiera, necesito TLGenerator
spillTemp :: (TLGenerator a, Monad a) => [Assem] -> Frame -> Temp -> a ([Assem],Frame)
spillTemp [] fr t = return ([],fr{actualReg = actualReg fr +1})
spillTemp (op@(Oper _ dest src  _):ops) fr t = do
    let define = elem t dest
    let usa = elem t src
    (ops',nuevoFrame) <- spillTemp ops fr t
    if(usa || define)
    then do
        te <- newTemp
        let previa = if usa then [retrieveTemp te fr] else []
        let siguiente = if define then [saveTemp te fr] else []
        return (previa ++ [cambiarTemp op t te] ++ siguiente ++ ops', nuevoFrame)
    else return (op:ops',nuevoFrame)
spillTemp (op@(Mov _ dest src  ):ops) fr t = do
    let define =  t ==  dest
    let usa = t == src
    (ops',nuevoFrame) <- spillTemp ops fr t
    if(usa || define) 
    then do
        te <- newTemp 
        let previa = if usa then [retrieveTemp te fr] else []
        let siguiente = if define then [saveTemp te fr] else []
        return (previa ++ [cambiarTemp op t te] ++ siguiente ++ ops', nuevoFrame)
    else return (op:ops',nuevoFrame)
spillTemp (op:ops) fr t = do
    (ops',nuevoFrame) <- spillTemp ops fr t
    return (op:ops',nuevoFrame)


filtrarPrecoloreados :: (Allocator a) => [(Nodo, Nodo)] -> a [(Nodo, Nodo)]
filtrarPrecoloreados [] = return []
filtrarPrecoloreados ((n1,n2):ns) = do
    pre1 <- precolored n1
    pre2 <- precolored n2
    if(not pre1 || not pre2) --Se puede hacer coalesce siempre que ALGUNO no esté precoloreado. Ambos no pueden estarlo.
    then do 
        ns' <- filtrarPrecoloreados ns
        return $ (n1,n2):ns'
    else 
        filtrarPrecoloreados ns

--Auxs: 
--Toma el grafo de interferencia de liveness y lo convierte en uno de registros,
liftGrafo :: Grafo Temp -> Grafo Nodo
liftGrafo (Grafo vertices aristas) = G.Grafo (S.map (\a -> S.singleton a) vertices) (S.map (\(a,b)-> (S.singleton a, S.singleton b)) aristas)

reemplazar :: (Eq a) => a -> a -> a -> a
reemplazar x y z = if z == x then y else z

reemplazarL :: (Eq a) => [a] -> a -> a -> a
reemplazarL xs y z = if (L.elem z xs) then y else z

findMoves :: [Assem] -> Set (Nodo, Nodo)
findMoves [] = S.empty
findMoves ((Mov assem dest src):as) = S.insert (S.singleton dest,S.singleton src)  (findMoves as)
findMoves (a:as) = findMoves as

coloresBase :: Map Temp Temp
coloresBase = M.fromAscList $ P.zip todosLosRegistros todosLosRegistros

colorear :: Nodo -> Map Temp Temp -> Maybe Temp
colorear n m = primerValor $ S.toList $ S.map (\t -> M.lookup t $ M.fromList $ M.toAscList m) n
 where
    primerValor [] = Nothing
    primerValor (Nothing:vs) = primerValor vs
    primerValor ((Just v):vs) = Just v

setMaybes :: (Ord a) => Set (Maybe a) -> Set a
setMaybes s = S.fromList $ MB.catMaybes $ S.toList s

insertarColor :: Map Temp Temp -> Nodo -> Temp -> Map Temp Temp
insertarColor mapa nodo color = P.foldl (\m (k,v) -> M.insert k v m) mapa $ P.zip (S.toList nodo) (repeat color)

--No me gusta el warning
mkState :: Integer -> AllocState
mkState unique = AS {
    stack = []
    ,interf = Grafo (S.empty) (S.empty)
    ,moves =  S.empty
    ,simplify = []
    ,coalesce = []
    ,freeze = []
    ,spill = []
    ,unique = unique
}

allocate :: [Assem] -> Frame -> Integer -> ([Assem], Frame)
allocate assembly frame unique = fst $ runState (allocMain assembly frame) (mkState unique)