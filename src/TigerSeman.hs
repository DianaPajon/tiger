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

buscarM :: Symbol -> [(Symbol, Tipo, Int)] -> Maybe Tipo
buscarM s [] = Nothing
buscarM s ((s',t,_):xs) | s == s' = Just t
                        | otherwise = buscarM s xs

-- | __Completar__ 'transVar'.
-- El objetivo de esta función es obtener el tipo
-- de la variable a la que se está __accediendo__.
-- ** transVar :: (MemM w, Manticore w) => Var -> w (BExp, Tipo)
transVar :: (MemM w, Manticore w) => Var -> w ( BExp , Tipo)
transVar (SimpleVar s)      = do (t,a,i) <- getTipoValV s -- Nota [1]
                                 nil <- nilExp
                                 return (nil, t)
transVar (FieldVar v s)     = do (e, tBase) <- transVar v
                                 nil <- nilExp
                                 case tBase of 
                                  TRecord fs u -> 
                                    case buscarM s fs of
                                      Just t -> return (nil, t)
                                      Nothing -> derror $ pack  "No se encontró el campo."
                                  _ -> derror $ pack "No es un record"
transVar (SubscriptVar v e) = do (e, tBase ) <- transVar v
                                 nil <- nilExp
                                 case tBase of
                                  TArray t u -> return (nil, t)
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
noBadLoops :: [(Symbol,Ty,Pos)]  -> [(Symbol,Ty,Pos)] -> Bool
noBadLoops [] ys = True
noBadLoops ((s,NameTy sim,p):xs) ys = (P.foldr (\(s,t,p) b-> sim /= s && b) True (((s,NameTy sim,p):xs) ++ ys))
                                      && ( noBadLoops xs ((s, NameTy sim, p):ys))
noBadLoops ((s,t,p):xs) ys = noBadLoops xs ((s,t,p):ys)

-- Esta función se encarga de eliminar la gramatica del ABS y pasar
-- las definiciones de tipo a la gramatica interna del compilador.
-- Los tipos estan referidos, 
preTy :: (Manticore w) => (Symbol, Ty, Pos) -> w (Symbol, Tipo)
preTy (sim,(NameTy s),p)      = return (sim, TTipo s)
preTy (sim,(RecordTy flds),p) = do unique <- ugen
                                   return (sim, TRecord (P.map (\(sim,NameTy s) -> (sim, RefRecord s, 0))  flds) unique)
preTy (sim,(ArrayTy s),p)     = do unique <- ugen
                                   return (sim, TArray (TTipo s) unique)


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
transDecs :: (MemM w, Manticore w) => [Dec] -> w (BExp,Tipo) -> w (BExp,Tipo)
transDecs ((VarDec nm escap t init p): xs) exp = do 
  nil <- nilExp
  acceso <- allocLocal (escap == Escapa)
  nivel <- getActualLevel
  (u, tipoExp) <- transExp init
  tipoDeclarado <- case t of
                    Just s -> getTipoT s
                    Nothing -> (return TUnit)
  iguales <- tiposIguales tipoExp tipoDeclarado
  if (tipoDeclarado == TUnit || iguales)
  then transDecs xs (insertValV nm (tipoExp, acceso, fromIntegral nivel) exp)
  else derror $ pack "El tipo declarado no conicide con el de la expresión dada"
transDecs ((TypeDec xs): xss)             exp = 
  -- REVISAR COMPLETAMENTE ESTE CODIGO, ES UNA MUGRE. EN SERIO, MUGRE.
  -- Lo único bueno es que el manejo de accesos y niveles no puede nunca estar mal...
  if (noBadLoops xs [])
  then do tys <- mapM preTy xs
          let sims = P.map fst tys
          clean <- cleanTys sims tys
          let decs = P.map (\(s,t) ->(s,arreglarLazy t clean)) clean
          transDecs xss (P.foldr (\(s,t) e -> insertTipoT s t e) (exp) decs)
  else derror $ pack "Tipos recursivos mal declarados"
transDecs ((FunctionDec fs) : xs)          exp = do 
  funEntries <- mapM mkFunEntry fs
  funs <- mapM (transFun funEntries) fs
  transDecs xs (P.foldr (\(s,fentry) e -> insertFunV s fentry e)  exp (actualizar funEntries funs))
    where actualizar [] xs = []
          actualizar ((s,(l,a,b,c,d)):es) ((ci,t,l'):ts) = (s,(l',a,b,c,d)) : actualizar es ts

type FunDec = (Symbol ,[(Symbol, Escapa, Ty)], Maybe Symbol, Exp, Pos)

transFun :: (MemM w, Manticore w) => [(Symbol, FunEntry)] -> FunDec -> w (BExp, Tipo, Level)
transFun fs (nombre, args, mt, body, _) = do 
  pushLevel (nivelFuncion fs nombre)
  args <- mapM mkArgEntry args
  (intermedio, tipo) <- P.foldr (\(s,argentry) e  -> insertValV s argentry e) expresionConFuns args
  levelConArgs <- topLevel
  popLevel
  case mt of 
    Nothing -> return (intermedio,TUnit,levelConArgs)
    Just t -> do tipoEsperado <- getTipoT t
                 iguales <- tiposIguales tipoEsperado tipo
                 if iguales
                  then return (intermedio, tipo, levelConArgs)
                  else derror $ pack "La función no tipa"
    where 
      nivelFuncion ((nombre, (level,_,_,_,_)):funs) s = if s == nombre then level else nivelFuncion funs s
      expresionConFuns = P.foldr (\(s,fentry) e -> insertFunV s fentry e)  (transExp body) fs


mkArgEntry :: (MemM w, Manticore w) => (Symbol,Escapa,Ty) -> w (Symbol, ValEntry)
mkArgEntry (s,e,t) = do
  acceso <- allocArg (e == Escapa)
  tipo <- fromTy t
  unique <- ugen
  return (s,(tipo, acceso, fromIntegral unique))

mkFunEntry :: (MemM w, Manticore w) => FunDec -> w (Symbol, FunEntry)
mkFunEntry (nombre,args,mtipo,cuerpo,pos) = do 
  nivelPadre <- topLevel
  let formals = True : (P.map (\(a,b,c) -> b == Escapa) args) -- Agrego el static link
  tipos <- mapM transTy (P.map (\(a,b,c) -> c) args)
  label <- newLabel
  tipo <- case mtipo of
    Nothing -> return TUnit
    Just s -> getTipoT s
  let nivelFuncion = newLevel nivelPadre label formals
  return (nombre,(nivelFuncion, label, tipos, tipo, Propia))
 

getTipoEntry :: (Unique, Label, [Tipo], Tipo, Externa) -> Tipo
getTipoEntry (u,l,ts,t,e) = t

-- ** transExp :: (MemM w, Manticore w) => Exp -> w (BExp , Tipo)
transExp :: (MemM w, Manticore w) => Exp -> w (BExp , Tipo)
transExp (VarExp v p) = addpos (transVar v) p
transExp UnitExp{} = do nil <- nilExp
                        return (nil, TUnit) -- ** fmap (,TUnit) unitExp
transExp NilExp{} = do nil <- nilExp
                       return (nil, TNil) -- ** fmap (,TNil) nilExp
transExp (IntExp i _) = do nil <- nilExp
                           return (nil, TInt RW) -- ** fmap (,TInt RW) (intExp i)
transExp (StringExp s _) = do nil <- nilExp
                              return (nil , TString) -- ** fmap (,TString) (stringExp (pack s))
transExp (CallExp nm args p) = undefined -- Completar
transExp (OpExp el' oper er' p) = do -- Esta va /gratis/
        (_ , el) <- transExp el'
        (_ , er) <- transExp er'
        case oper of
          EqOp -> if tiposComparables el er EqOp then oOps el er
                  else addpos (derror (pack "Error de Tipos. Tipos no comparables")) p
          NeqOp ->if tiposComparables el er EqOp then oOps el er
                  else addpos (derror (pack "Error de Tipos. Tipos no comparables")) p
          -- Los unifico en esta etapa porque solo chequeamos los tipos, en la próxima
          -- tendrán que hacer algo más interesante.
          PlusOp -> oOps el er
          MinusOp -> oOps el er
          TimesOp -> oOps el er
          DivideOp -> oOps el er
          LtOp -> oOps el er
          LeOp -> oOps el er
          GtOp -> oOps el er
          GeOp -> oOps el er
          where oOps l r = if equivTipo l r -- Chequeamos que son el mismo tipo
                              && equivTipo l (TInt RO) -- y que además es Entero. [Equiv Tipo es una rel de equiv]
                           then do nil <- nilExp; return (nil, TInt RO)
                           else addpos (derror (pack "Error en el chequeo de una comparación.")) p
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
        fldsTys <- mapM (\(nm, cod) -> (nm,) <$> transExp cod) flds -- Buscamos los tipos de cada una de los campos.
        -- como resultado tenemos 'fldsTys :: (Symbol, ( CIr , Tipo))'
        -- Lo que resta es chequear que los tipos  sean los mismos, entre los que el programador dio
        -- y los que tienen que ser según la definición del record.
        let ordered = List.sortBy (Ord.comparing fst) fldsTys
        -- asumiendo que no nos interesan como el usuario ingresa los campos los ordenamos.
        _ <- flip addpos p $ cmpZip ( (\(s,(c,t)) -> (s,t)) <$> ordered) fldsTy -- Demon corta la ejecución.
        nil <- nilExp
        return (nil, trec) -- Si todo fue bien devolvemos trec.
    _ -> flip addpos p $ derror (pack "Error de tipos.")
  )
transExp(SeqExp es p) = fmap last (mapM transExp es)
  -- last <$> mapM transExp es
-- ^ Notar que esto queda así porque no nos interesan los
-- units intermedios. Eventualmente vamos a coleccionar los códigos intermedios y se verá algo similar a:
-- do
--       es' <- mapM transExp es
--       return ( () , snd $ last es')
transExp(AssignExp var val p) = 
  do (_, tipoVar) <- transVar var
     (_, tipoExp) <- transExp val
     iguales <- tiposIguales tipoVar tipoExp
     nil <- nilExp
     if (iguales)
      then return (nil, TNil)
      else derror $ pack ("Los tipos no coinciden en la asignaciòn, linea " ++ show p)
transExp(IfExp co th Nothing p) = do
        -- ** (ccond , co') <- transExp co
  -- Analizamos el tipo de la condición
        (_ , co') <- transExp co
  -- chequeamos que sea un entero.
        unless (equivTipo co' TBool) $ errorTiposMsg p "En la condición del if->" co' TBool -- Claramente acá se puede dar un mejor error.
        -- ** (cth , th') <- transExp th
  -- Analizamos el tipo del branch.
        (u , th') <- transExp th
  -- chequeamos que sea de tipo Unit.
        unless (equivTipo th' TUnit) $ errorTiposMsg p "En el branch del if->" th' TUnit
  -- Si todo fue bien, devolvemos que el tipo de todo el 'if' es de tipo Unit.
        nil <- nilExp
        return (nil , TUnit)
transExp(IfExp co th (Just el) p) = do
  (_ , condType) <- transExp co
  unless (equivTipo condType TBool) $ errorTiposMsg p "En la condición del if ->" condType TBool
  (_, ttType) <- transExp th
  (_, ffType) <- transExp el
  C.unlessM (tiposIguales ttType ffType) $ errorTiposMsg p "En los branches." ttType ffType
  -- Si todo fue bien devolvemos el tipo de una de las branches.
  nil <- nilExp
  return (nil, ttType)
transExp(WhileExp co body p) = do
  (_ , coTy) <- transExp co
  unless (equivTipo coTy TBool) $ errorTiposMsg p "Error en la condición del While" coTy TBool
  (_ , boTy) <- transExp body
  unless (equivTipo boTy TUnit) $ errorTiposMsg p "Error en el cuerpo del While" boTy TBool
  nil <- nilExp
  return (nil, TUnit)
transExp(ForExp nv mb lo hi bo p) = -- Hack que da el analisis semántico, no es realmente la forma de generar codigo intermedio
  transDecs [(VarDec nv mb Nothing lo p)] (transExp bo)
transExp(LetExp dcs body p) = transDecs dcs (transExp body)
transExp(BreakExp p) =
  do nil <- nilExp
     return (nil, TUnit)
transExp(ArrayExp sn cant init p) = do -- Completar
  tipoUsado <- getTipoT sn
  case tipoUsado of
    TArray t u -> do
      (_, tipoCant) <- transExp cant
      (_, tipoInit) <- transExp init
      cantEntera <- tiposIguales tipoCant (TInt RO)
      tipoValido <- tiposIguales tipoInit t
      nil <- nilExp
      if (cantEntera && tipoValido)
        then return (nil,TArray t u)
        else derror $ pack ("Arreglo mal declarado, linea " ++ show p)
    _ -> derror $ pack "Se declara un array de un tipo no array"
  


runSeman = undefined
