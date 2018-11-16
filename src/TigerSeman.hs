{-# LANGUAGE TupleSections #-}
module TigerSeman where

import           TigerAbs
import           TigerErrores               as E
import           TigerSres
import           TigerSymbol
import           TigerTips
import           TigerUnique

-- Segunda parte imports:
import           TigerTemp
import           TigerTrans
import           TigerFrame                 (Frag(Proc))

-- Monads
import qualified Control.Conditional        as C
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Except

-- Data
import           Data.List                  as List
import           Data.Map                   as M
import           Data.Ord                   as Ord

-- Le doy nombre al Preludio.
import           Prelude                    as P

-- Debugging. 'trace :: String -> a -> a'
-- imprime en pantalla la string cuando se ejecuta.
import           Debug.Trace                (trace)
-- * Análisis Semántico, aka Inferidor de Tipos

-- ** Notas :

-- [1] No deberían fallar las búsquedas de variables. Recuerden que
-- el calculo de variables escapadas debería detectar las variables
-- no definidas.

-- [2] En la siguiente etapa vamos a ir generando el código intermedio
-- mezclado con esta etapa por lo que es muy posible que tengan que revisar
-- este modulo. Mi consejo es que sean /lo más ordenados posible/ teniendo en cuenta
-- que van a tener que reescribir bastante.

class (Demon w, Monad w, MemM w) => Manticore w where
  -- | Inserta una Variable al entorno
    insertValV :: Symbol -> ValEntry -> w a -> w a
  -- | Inserta una Función al entorno
    insertFunV :: Symbol -> FunEntry -> w a -> w a
  -- | Inserta una Variable de sólo lectura al entorno
    insertVRO :: Symbol -> w a -> w a
  -- | Inserta una variable de tipo al entorno
    insertTipoT :: Symbol -> Tipo -> w a -> w a
  -- | Busca una función en el entorno
    getTipoFunV :: Symbol -> w FunEntry
  -- | Busca una variable en el entorno. Ver [1]
    getTipoValV :: Symbol -> w ValEntry
  -- | Busca un tipo en el entorno
    getTipoT :: Symbol -> w Tipo
  -- | Funciones de Debugging!
    showVEnv :: w a -> w a
    showTEnv :: w a -> w a
    --
    -- | Función monadica que determina si dos tipos son iguales.
    -- El catch está en que tenemos una especie de referencia entre los
    -- nombres de los tipos, ya que cuando estamos analizando la existencia de bucles
    -- en la definición permitimos cierto alias hasta que los linearizamos con el
    -- sort topológico.
    tiposIguales :: Tipo -> Tipo -> w Bool
    tiposIguales (RefRecord s) l@(TRecord _ u) = do
        st <- getTipoT s
        case st of
            TRecord _ u1 -> return (u1 == u)
            ls@RefRecord{} -> tiposIguales ls l
            _ -> E.internal $ pack "No son tipos iguales... 123+1"
    tiposIguales l@(TRecord _ u) (RefRecord s) = do
        st <- getTipoT s
        case st of
            TRecord _ u1 -> return (u1 == u)
            ls@RefRecord{} -> tiposIguales l ls
            _ -> E.internal $ pack "No son tipos iguales... 123+2"
    tiposIguales (RefRecord s) (RefRecord s') = do
        s1 <- getTipoT s
        s2 <- getTipoT s'
        tiposIguales s1 s2
    tiposIguales TNil  (RefRecord _) = return True
    tiposIguales (RefRecord _) TNil = return True
    tiposIguales (RefRecord _) _ = E.internal $ pack "No son tipos iguales... 123+3"
    tiposIguales  e (RefRecord s) = E.internal $ pack $ "No son tipos iguales... 123+4" ++ (show e ++ show s)
    tiposIguales a b = return (equivTipo a b)
    --
    -- | Generador de uniques.
    --
    ugen :: w Unique



-- | Definimos algunos helpers

-- | `addpos` nos permite agregar información al error.
addpos :: (Demon w, Show b) => w a -> b -> w a
addpos t p = E.adder t (pack $ show p)

-- | Patrón de errores...
errorTiposMsg :: (Demon w, Show p)
              => p -> String -> Tipo -> Tipo -> w a
errorTiposMsg p msg t1 t2 = flip addpos p
    $ flip adder (pack msg)
    $ errorTipos t1 t2

depend :: Ty -> [Symbol]
depend (NameTy s)    = [s]
depend (ArrayTy s)   = [s]
depend (RecordTy ts) = concatMap (depend . snd) ts


-- | Función auxiliar que chequea cuales son los tipos
-- comparables.
-- Por ejemplo, ` if nil = nil then ...` es una expresión ilegal
-- ya que no se puede determinar el tipo de cada uno de los nils.
-- Referencia: [A.3.Expressions.Nil]
tiposComparables :: Tipo -> Tipo -> Oper -> Bool
tiposComparables TNil TNil EqOp  = False
tiposComparables TUnit _ EqOp    = False
tiposComparables _ _ EqOp        = True
tiposComparables TNil TNil NeqOp = False
tiposComparables TUnit _ NeqOp   = False
tiposComparables _ _ NeqOp       = True
tiposComparables _ _ _           = True

-- | Función auxiliar que indica que operadores son de comparación
comparacion :: Oper -> Bool
comparacion PlusOp = False 
comparacion MinusOp = False
comparacion TimesOp = False 
comparacion DivideOp = False
comparacion _ = True

-- | Función auxiliar que indica que operadores son de igualdad
igualdad :: Oper -> Bool
igualdad NeqOp = True
igualdad EqOp = True
igualdad _ = False

diferentes :: (Eq a) => [a] -> Bool
diferentes [] = True
diferentes (a:xs) = not (elem a xs) && diferentes xs

-- | Función que chequea que los tipos de los campos sean los mismos
-- Ver 'transExp (RecordExp ...)'
-- Ver 'transExp (CallExp ...)'
cmpZip :: (Demon m, Monad m) => [(Symbol, Tipo)] -> [(Symbol, Tipo, Int)] -> m () --Bool
cmpZip [] [] = return ()
cmpZip [] _ = derror $ pack "Diferencia en la cantidad. 1"
cmpZip _ [] = derror $ pack "Diferencia en la cantidad. 2"
cmpZip ((sl,tl):xs) ((sr,tr,p):ys) =
        if (equivTipo tl tr && sl == sr)
        then cmpZip xs ys
        else errorTipos tl tr

buscarM :: Symbol -> [(Symbol, Tipo, Int)] -> Maybe (Symbol, Tipo, Int)
buscarM s [] = Nothing
buscarM s ((s',t,i):xs) | s == s' = Just (s',t,i)
                        | otherwise = buscarM s xs

-- | __Completar__ 'transVar'.
-- El objetivo de esta función es obtener el tipo
-- de la variable a la que se está __accediendo__.
-- ** transVar :: (MemM w, Manticore w) => Var -> w (BExp, Tipo)
transVar :: (MemM w, Manticore w) => Var -> w ( BExp , Tipo)
transVar (SimpleVar s)      = do (t,a,i) <- getTipoValV s -- Nota [1]
                                 var <- simpleVar a i
                                 return (var, t)
transVar (FieldVar v s)     = do (var, tBase) <- transVar v
                                 case tBase of 
                                  TRecord fs u -> 
                                    case buscarM s fs of
                                      Just (s,t,i) -> do
                                        campo <- fieldVar var i
                                        return (campo, t)
                                      Nothing -> derror $ pack  "No se encontró el campo."
                                  _ -> derror $ pack "No es un record"
transVar (SubscriptVar v e) = do (var, tBase) <- transVar v
                                 case tBase of
                                  TArray t u -> do
                                    (indice, tipo) <- transExp e
                                    case tipo of
                                      TInt _ -> do 
                                        campo <- subscriptVar var indice
                                        return (campo, t)
                                      _ ->  errorTipos tipo $ TInt RW
                                  _ -> derror $ pack "No es un array"

-- | __Completar__ 'TransTy'
-- El objetivo de esta función es dado un tipo
-- que proviene de la gramatica, dar una representación
-- de tipo interna del compilador

-- | Nota para cuando se generte código intermedio
-- que 'TransTy ' no necesita ni 'MemM ' ni devuelve 'BExp'
-- porque no se genera código intermedio en la definición de un tipo.
transTy :: (Manticore w) => Ty -> w Tipo
transTy (ArrayTy s) =
  do
    unique <- ugen
    tipo <- getTipoT s
    return $ TArray tipo unique
transTy (RecordTy flds) = 
  do 
    let simbolos = P.map fst flds
    tipos <- mapM (transTy) (P.map snd flds)
    unique <- ugen
    let componentes = triZip simbolos tipos (repeat 0)
    return $ TRecord componentes unique
transTy (NameTy s) = --Es importante diferenciar las 
  getTipoT s

-- Esta funcion se encarga de detectar los bucles ilegales en la lista de tipos definidos
hayCiclos :: [(Symbol, Tipo)] -> Bool
hayCiclos tips = P.foldr (\bl bb -> bl || bb) False listaCiclos
 where 
  nameTys = P.filter (\(s,t) -> esNameTy t)  tips
  esNameTy t = case t of
                    TTipo _ -> True
                    _ -> False
  listaCiclos = P.map (\(a,TTipo b) -> hayCiclo (hacerHead a nameTys) []) nameTys

hayCiclo :: [(Symbol,Tipo)] -> [Symbol] -> Bool
hayCiclo ((a, TTipo b):ts) ls = P.elem b ls || hayCiclo (hacerHead b ts) (a:ls)
hayCiclo [] ls = False

hacerHead :: Symbol -> [(Symbol, Tipo)] -> [(Symbol, Tipo)]
hacerHead b ((c,t):ts) = if b == c then (c,t):ts else (hacerHead b ts ++ [(c,t)])
hacerHead b [] = []

-- Esta función se encarga de eliminar la gramatica del ABS y pasar
-- las definiciones de tipo a la gramatica interna del compilador.
-- Los tipos estan referidos, 
preTy :: (Manticore w) => (Symbol, Ty, Pos) -> w (Symbol, Tipo)
preTy (sim,(NameTy s),p)      = addpos (return (sim, TTipo s)) p
preTy (sim,(RecordTy flds),p) = 
  flip addpos p (
    do
      unique <- ugen
      return (sim, TRecord (P.map (\(sim,NameTy s) -> (sim, RefRecord s, posicion sim flds))  flds) unique)
  ) where
      posicion s ((s', t):xs) = if s == s' then 0 else 1 + posicion s xs    

preTy (sim,(ArrayTy s),p)     = 
  flip addpos p (
    do
      unique <- ugen
      return (sim, TArray (TTipo s) unique)
  )


-- Dada una lista de definiciones de la función anterior, detecta que tipos se refieren entre si
-- y reemplaza el resto.
cleanTy :: (Manticore w) => [Symbol] -> Tipo -> w Tipo
cleanTy sims (RefRecord s) = 
  if elem s sims 
  then return $ RefRecord s
  else getTipoT s
cleanTy sims (TTipo s) = 
  if elem s sims 
  then return $ TTipo s
  else getTipoT s
cleanTy sims (TArray t u) = 
  do ct <- cleanTy sims t
     return (TArray ct u)
cleanTy sims (TRecord ts u) = 
  do let tiposRecord = P.map (\(a,b,c) -> b) ts
     ctipos <- mapM (cleanTy sims) tiposRecord
     let nuevosTiposRecord = zipWith (\(a,b,c) t -> (a,t,c)) ts ctipos
     return $ TRecord nuevosTiposRecord u
cleanTy sims lala = return lala

-- | Dada una lista de definiciones de la función anterior, detecta que tipos se refieren entre si
-- y reemplaza el resto.
cleanTys :: (Manticore w) => [Symbol] -> [(Symbol, Tipo)] -> w [(Symbol, Tipo)]
cleanTys ssims [] = return []
cleanTys sims ((s,t) : sts) = 
  do tip <- cleanTy sims t
     tipos <- cleanTys sims sts
     return ((s,tip):tipos)

elemTupla :: Eq a => a -> [(a, b)] -> b
elemTupla s ((ss,t):ts) = if s == ss then t else elemTupla s ts
     
-- | Esta función, dada la definicón anterior, genera los tipos REALES del compilador
-- como valores lazy para guardar en un mapa.
arreglarLazy :: Tipo -> [(Symbol, Tipo)] -> Tipo
arreglarLazy (RefRecord s) tipos = arreglarLazy (elemTupla s tipos) tipos
arreglarLazy (TTipo s) tipos = arreglarLazy (elemTupla s tipos) tipos
arreglarLazy (TRecord ts u) tipos = TRecord (P.map (\(s,t,i) -> (s, arreglarLazy t tipos,i)) ts) u
arreglarLazy (TArray t u) tipos = TArray (arreglarLazy t tipos) u
arreglarLazy lala tipos = lala



triZip :: [a] -> [b] -> [c] -> [(a,b,c)]
triZip as bs cs = P.map (\((a,b),c) -> (a,b,c)) (zip (zip as bs) cs)


fromTy :: (Manticore w) => Ty -> w Tipo
fromTy (NameTy s) = getTipoT s
fromTy _ = P.error "no debería haber una definición de tipos en los args..."


-- | Tip: Capaz que se debería restringir el tipo de 'transDecs'.
-- Tip2: Van a tener que pensar bien que hacen. Ver transExp (LetExp...)
-- ** transDecs :: (MemM w, Manticore w) => [Dec] -> w a -> w a
transDecs' :: (MemM w, Manticore w) => [Dec] -> w (BExp,Tipo) -> w ((BExp,Tipo),[BExp])
transDecs' ((VarDec nm escap t init p): xs) exp = flip addpos p (do 
  nil <- nilExp
  acceso <- allocLocal (escap == Escapa)
  nivel <- getActualLevel
  variable <- varDec acceso
  (inicializacion, tipoExp) <- transExp init
  let tipoInit = if tipoExp == TInt RO then TInt RW else tipoExp
  asignacion <- assignExp variable inicializacion
  tipoDeclarado <- case t of
                    Just s -> getTipoT s
                    Nothing -> return tipoInit
  iguales <- tiposIguales tipoExp tipoDeclarado
  case (iguales, tipoDeclarado == TNil) of
    (True, False) -> do 
      (cuerpo, inits) <- insertValV nm (tipoDeclarado, acceso, fromIntegral nivel) (transDecs' xs  exp)
      return (cuerpo, asignacion : inits)
    (True, True) ->  derror $ pack "Se debe declarar el tipo para poder asignar nil"
    (False, _) -> derror $ pack "El tipo declarado no conicide con el de la expresión dada" )

transDecs' ((TypeDec xs): xss)             exp = do
  tys <- mapM preTy xs
  let sims = P.map fst tys
  clean <- cleanTys sims tys
  let noRepiten = diferentes sims
  if ((not $ hayCiclos clean) && noRepiten)
  then do let decs = P.map (\(s,t) ->(s,arreglarLazy t clean)) clean
          P.foldr (\(s,t) e -> insertTipoT s t e) (transDecs' xss exp) decs
  else do let (_,_,p) = head xs
          addpos (derror $ pack "Bloque de tipos mal declarados") p
transDecs' ((FunctionDec fs) : xs)          exp = do 
  let noRepiten = diferentes $ P.map (\(s,_,_,_,_) -> s) fs
  if(noRepiten)
    then do
      funEntries <- mapM mkFunEntry fs
      funs <- mapM (transFun funEntries) fs --Ignoro los cuerpos de las definiciones por ahora.
      P.foldr (\(s,fentry) e -> insertFunV s fentry e)  (transDecs' xs exp) (actualizar funEntries funs)
    else let (_,_, _, _, p) = head fs 
          in addpos (derror $ pack "Hay varias funciones con el mismo nombre declaradas en un solo bloque.") p
  where actualizar [] xs = []
        actualizar ((s,(l,a,b,c,d)):es) ((ci,t,l'):ts) = (s,(l',a,b,c,d)) : actualizar es ts
transDecs' [] exp = do
  cuerpo <- exp
  return (cuerpo, [])

transDecs :: (MemM w, Manticore w) => [Dec] -> w (BExp,Tipo) -> w (BExp,Tipo)
transDecs decs cuerpo = do
  ((cuerpo,tipo), asignaciones) <- transDecs' decs cuerpo
  cuerpoLet <- letExp asignaciones cuerpo
  return (cuerpoLet, tipo)

type FunDec = (Symbol ,[(Symbol, Escapa, Ty)], Maybe Symbol, Exp, Pos)

transFun :: (MemM w, Manticore w) => [(Symbol, FunEntry)] -> FunDec -> w (BExp, Tipo, Level)
transFun fs (nombre, args, mt, body, p) = 
  flip addpos p (
    do 
      let nivelFun = nivelFuncion fs nombre
      pushLevel nivelFun
      args <- mapM mkArgEntry args
      let expresionConArgs = P.foldr (\(s,argentry) e  -> insertValV s argentry e) (transExp body) args
      (cuerpo , tipo) <- P.foldr (\(s,fentry) e -> insertFunV s fentry e) expresionConArgs fs
      let isproc = if mt == Nothing then IsProc else IsFun
      intermedio <- envFunctionDec nivelFun (functionDec cuerpo nivelFun isproc)
      levelConArgs <- topLevel
      popLevel
      case mt of 
        Nothing -> do esProc <- tiposIguales TUnit tipo
                      if esProc then return (intermedio,TUnit,levelConArgs) else derror $ pack "Un procedimiento retorna un valor"
        Just t -> do tipoEsperado <- getTipoT t
                     iguales <- tiposIguales tipoEsperado tipo
                     if iguales
                       then return (intermedio, tipo, levelConArgs)
                       else derror $ pack "La función no tipa"
  )
 where 
  nivelFuncion ((nombre, (level,_,_,_,_)):funs) s = if s == nombre then level else nivelFuncion funs s

mkArgEntry :: (MemM w, Manticore w) => (Symbol,Escapa,Ty) -> w (Symbol, ValEntry)
mkArgEntry (s,e,t) = do
  acceso <- allocArg (e == Escapa)
  tipo <- fromTy t
  unique <- ugen
  return (s,(tipo, acceso, fromIntegral unique))

mkFunEntry :: (MemM w, Manticore w) => FunDec -> w (Symbol, FunEntry)
mkFunEntry (nombre,args,mtipo,cuerpo,pos) = do 
  nivelPadre <- topLevel
  let formals = (P.map (\(a,b,c) -> b == Escapa) args) 
  tipos <- mapM transTy (P.map (\(a,b,c) -> c) args)
  label <- newLabel
  tipo <- case mtipo of
    Nothing -> return TUnit
    Just s -> getTipoT s
  let nivelFuncion = newLevel nivelPadre label formals
  return (nombre,(nivelFuncion, label, tipos, tipo, Propia))
 

getTipoEntry :: (Unique, Label, [Tipo], Tipo, Externa) -> Tipo
getTipoEntry (u,l,ts,t,e) = t

normalizarRecord :: (Manticore w ) => Exp -> w Exp
normalizarRecord (RecordExp flds rt p) = do
  (TRecord fldsTy _) <- getTipoT rt --TODO: Bad fail alert.
  let ordFldsTy = sortBy (\(a,b,c) (a',b',c') -> compare c c') fldsTy
  let ordFldsRec = sortBy (\(a,b) (a',b') -> compare (posicion a fldsTy) (posicion a' fldsTy)) flds
  return $ RecordExp ordFldsRec rt p
 where posicion elem ((a,b,c):es) = if a == elem then 0 else 1 + posicion elem es
  

-- ** transExp :: (MemM w, Manticore w) => Exp -> w (BExp , Tipo)
transExp :: (MemM w, Manticore w) => Exp -> w (BExp , Tipo)
transExp (VarExp v p) = addpos (transVar v) p
transExp UnitExp{} =  fmap (,TUnit) unitExp
transExp NilExp{} =  fmap (,TNil) nilExp
transExp (IntExp i _) = fmap (,TInt RW) (intExp i)
transExp (StringExp s _) =  fmap (,TString) (stringExp (pack s))
transExp (CallExp nm args p) = flip addpos p (do
  transArgs <- mapM transExp args
  let tiposArgs = P.map snd transArgs
  let argumentos = P.map fst transArgs
  (level, label, parametros, tipo, externa) <- getTipoFunV nm
  iguales <- mapM (\(t1,t2) -> tiposIguales t1 t2) $ zip tiposArgs parametros
  let tipa = P.foldr (\a b -> a && b) True iguales
  let isProc = if tipo == TUnit then IsProc else IsFun
  if tipa && P.length parametros == P.length argumentos
  then do llamada <- callExp label externa isProc level argumentos
          return (llamada, tipo)
  else derror $ pack "La llamada a funcion no tipa."
 )
transExp (OpExp el' oper er' p) = flip addpos p (do -- Esta va /gratis/
  (leftExp, el) <- transExp el'
  (rightExp, er) <- transExp er'
  intermedio <- case (el, er) of
      (TInt _, TInt _) -> 
        if comparacion oper 
        then binOpIntRelExp leftExp oper rightExp
        else binOpIntExp leftExp oper rightExp
      (TString , TString) -> 
        if comparacion oper
        then binOpStrExp leftExp oper rightExp
        else derror $ pack "Operacion inválida"
      (TArray _ u1, TArray _ u2) -> 
        case (u1 == u2, igualdad oper) of
          (True, True) -> binOpIntRelExp leftExp oper rightExp --Es comparacion de instancias
          _ -> derror $ pack "Operacion inválida"
      (TRecord _ u1, tu2) -> 
        case tu2 of
          TNil -> if igualdad oper then  binOpIntRelExp leftExp oper rightExp else derror $ pack "Operacion inválida"
          TRecord _ u2 -> if u1 == u2 && igualdad oper then  binOpIntRelExp leftExp oper rightExp else derror $ pack "Operacion inválida"
          _ -> derror $ pack "Operacion inválida"
      (TNil, tu2) -> 
        case tu2 of
          TRecord _ u2 -> if igualdad oper then  binOpIntRelExp leftExp oper rightExp else derror $ pack "Operacion inválida"
          _ -> derror $ pack "Operacion inválida"
      _ -> derror $ pack "Operacion inválida"
  return (intermedio, TInt RW)
 )
-- | Recordemos que 'RecordExp :: [(Symbol, Exp)] -> Symbol -> Pos -> Exp'
-- Donde el primer argumento son los campos del records, y el segundo es
-- el texto plano de un tipo (que ya debería estar definido). Una expresión
-- de este tipo está creando un nuevo record.
transExp(RecordExp flds rt p) = 
  addpos (getTipoT rt) p >>= (
    \x -> case x of -- Buscamos en la tabla que tipo es 'rt', y hacemos un análisis por casos.
    trec@(TRecord fldsTy _) -> -- ':: TRecord [(Symbol, Tipo, Int)] Unique'
      do
        -- Especial atención acá.
        -- Tenemos una lista de expresiones con efectos
        -- y estos efectos tiene producirse en orden! 'mapM' viene a mano.
        (RecordExp fldsCanon _ _ ) <- normalizarRecord (RecordExp flds rt p)
        fldsTys <- mapM (\(nm, cod) -> (nm,) <$> transExp cod) fldsCanon
        flip addpos p $ cmpZip ( (\(s,(c,t)) -> (s,t)) <$> fldsTys) fldsTy -- Demon corta la ejecución.
        --let camposRecord = P.map (\(s, (exp, t)) -> (exp, pos s fldsTy)) fldsTys
        let camposRecord = P.map (\(s, (exp, t)) -> (exp, pos s fldsTy)) fldsTys
        --record <- return recordExp
        record <- recordExp camposRecord
        return (record, trec) -- Si todo fue bien devolvemos trec.
    _ -> flip addpos p $ derror (pack "Error de tipos en la definición del record.")
 )
  where pos s ((s',t,p):ts) = if s == s' then p else pos s ts
transExp(SeqExp es p) = do
       es' <- mapM transExp es
       seq <- seqExp $ P.map fst es'
       return ( seq , snd $ last es')
transExp(AssignExp var val p) = flip addpos p (do 
  (variable, tipoVar) <- transVar var
  (valor, tipoExp) <- transExp val
  let asignable = tipoVar /= TInt RO
  iguales <- tiposIguales tipoVar tipoExp
  case (asignable,iguales) of
    (False,_) -> derror $ pack ("La variable " ++ show var ++ " es de solo lectura")
    (True, False) -> derror $ pack ("Los tipos no coinciden en la asignaciòn" )
    (True, True) -> do 
      asignacion <- assignExp variable valor
      return (asignacion, TUnit)
 )
transExp(IfExp co th Nothing p) = do
        -- ** (ccond , co') <- transExp co
  -- Analizamos el tipo de la condición
        (condicion , co') <- transExp co
  -- chequeamos que sea un entero.
        unless (equivTipo co' TBool) $ errorTiposMsg p "En la condición del if->" co' TBool -- Claramente acá se puede dar un mejor error.
        -- ** (cth , th') <- transExp th
  -- Analizamos el tipo del branch.
        (cuerpo , th') <- transExp th
  -- chequeamos que sea de tipo Unit.
        unless (equivTipo th' TUnit) $ errorTiposMsg p "En el branch del if->" th' TUnit
  -- Si todo fue bien, devolvemos que el tipo de todo el 'if' es de tipo Unit.
        ifthen <- ifThenExp condicion cuerpo
        return (ifthen , TUnit)
transExp(IfExp co th (Just el) p) = do
  (condicion , condType) <- transExp co
  unless (equivTipo condType TBool) $ errorTiposMsg p "En la condición del if ->" condType TBool
  (cuerpoThen, ttType) <- transExp th
  (cuerpoElse, ffType) <- transExp el
  C.unlessM (tiposIguales ttType ffType) $ errorTiposMsg p "En los branches." ttType ffType
  -- Si todo fue bien devolvemos el tipo de una de las branches.
  ifthenelse <- ifThenElseExp condicion cuerpoThen cuerpoElse
  return (ifthenelse, ttType)
transExp(WhileExp co body p) = do
  (condicion , coTy) <- transExp co
  unless (equivTipo coTy TBool) $ errorTiposMsg p "Error en la condición del While" coTy TBool
  preWhileforExp
  (cuerpo , boTy) <- transExp body      
  unless (equivTipo boTy TUnit) $ errorTiposMsg p "Error en el cuerpo del While" boTy TUnit
  while <- whileExp condicion cuerpo
  posWhileforExp
  return (while, TUnit)
transExp(ForExp nv mb lo hi bo p) = 
  do
    --Genero la varEntry que necesito.
    acceso <- allocLocal False
    level <-getActualLevel
    insertValV nv (TInt RO, acceso, level) ( do
      (variable, tVariable) <- transExp (VarExp ( SimpleVar nv) (Simple 0 0))
      (minimo, tMinimo) <- transExp lo
      unless (equivTipo tMinimo (TInt RW)) $ errorTiposMsg p "El minimo del for no es un entero"  tMinimo (TInt RW)
      (maximo, tMaximo) <- transExp hi
      unless (equivTipo tMaximo (TInt RW)) $ errorTiposMsg p "El maximo del for no es un entero" tMaximo (TInt RW)
      preWhileforExp
      (cuerpo, tCuerpo) <- transExp bo
      unless (equivTipo tCuerpo TUnit) $ errorTiposMsg p "Error en el cuerpo del For" tCuerpo TUnit
      for <- forExp minimo maximo variable cuerpo
      posWhileforExp
      return (for, TUnit)
     )
transExp(LetExp dcs body p) = transDecs dcs (transExp body)
transExp(BreakExp p) =
  do break <- breakExp
     return (break, TUnit)
transExp(ArrayExp sn cant init p) = do 
  tipoUsado <- getTipoT sn
  case tipoUsado of
    TArray t u -> do
      (cantidad, tipoCant) <- transExp cant
      (init, tipoInit) <- transExp init
      cantEntera <- tiposIguales tipoCant (TInt RO)
      tipoValido <- tiposIguales tipoInit t
      if (cantEntera && tipoValido)
      then do
        array <- arrayExp cantidad init
        return (array,TArray t u)
      else derror $ pack ("Arreglo mal declarado, linea " ++ show p)
    _ -> derror $ pack "Se declara un array de un tipo no array"
  


transProg :: (MemM w, Manticore w) => Exp -> w [Frag]
transProg programa = do
  (programBody,tipoPrograma) <- transExp programa
  [level] <- topLevel
  proc <- functionDec  programBody [level] IsProc
  body <- unNx proc
  let frame = getFrame' level
  pushFrag $ Proc body frame
  frags <- getFrags
  return frags

runSeman = undefined