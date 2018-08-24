{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module TigerSeman where

import           TigerAbs
import           TigerErrores        as E
import           TigerSres
import           TigerTips

-- Segunda parte imports:
-- import           TigerTemp
-- import           TigerTrans

import           TigerSymbol

import           Control.Conditional as C
import           Control.Monad
import           Data.List           as List
import           Prelude             as P

import           Debug.Trace

class (Daemon w, Monad w) => Manticore w where
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
  -- | Busca una variable en el entorno
    getTipoValV :: Symbol -> w ValEntry
  -- NOTA: La busquedas de valores en los entorno
  -- no deberían fallar. Ya que se analizó en la etapa anterior.
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
    tiposIguales a b = return (intiposIguales a b)
    --
    -- | Generador de uniques.
    --
    ugen :: w Unique

-- | Definimos algunos helpers

addpos :: (Daemon w, Show b) => w a -> b -> w a
addpos t p = E.adder t (pack $ show p)


depend :: Ty -> [Symbol]
depend (NameTy s)    = [s]
depend (ArrayTy s)   = [s]
depend (RecordTy ts) = concatMap (depend . snd) ts


okOp :: Tipo -> Tipo -> Oper -> Bool
okOp TNil TNil EqOp  = False
okOp TUnit _ EqOp    = False
okOp _ _ EqOp        = True
okOp TNil TNil NeqOp = False
okOp TUnit _ NeqOp   = False
okOp _ _ NeqOp       = True
  -- Función incompleta, está bien esto?

cmpZip :: (Manticore m) =>  [(Symbol, Tipo)] -> [(Symbol, Tipo, Int)] -> m Bool
cmpZip [] [] = return True
cmpZip [] _ = return False
cmpZip _ [] = return False
cmpZip ((sl,tl):xs) ((sr,tr,p):ys) = do
        b <- tiposIguales tl tr
        if b  && (sl == sr) then cmpZip xs ys
                else return False

buscarM :: Symbol -> [(Symbol, Tipo, Int)] -> Maybe Tipo
buscarM s [] = Nothing
buscarM s ((s',t,_):xs) | s == s' = Just t
                        | otherwise = buscarM s xs

-- | Completar transVar.
-- El objetivo de esta función es obtener el tipo
-- de la variable a la que se está accediendo.
-- ** transVar :: (MemM w, Manticore w) => Var -> w (BExp, Tipo)
transVar :: (Manticore w) => Var -> w ( () , Tipo)
transVar (SimpleVar s)      = undefined
transVar (FieldVar v s)     = undefined
transVar (SubscriptVar v e) = undefined

-- | Completar TransTy
-- El objetivo de esta función es dado un tipo
-- que proviene de la gramatica, dar una representación
-- de tipo interna del compilador

-- | Notar que TransTy no necesita ni MemM ni devuelve BExp
-- porque no se genera código intermedio en la definición de un tipo.
transTy :: (Manticore w) => Ty -> w Tipo
transTy (NameTy s)      = undefined
transTy (RecordTy flds) = undefined
transTy (ArrayTy s)     = undefined


fromTy :: (Manticore w) => Ty -> w Tipo
fromTy (NameTy s) = getTipoT s
fromTy _ = P.error "no debería haber una definición de tipos en los args..."

-- | Tip: Capaz que se debería restringir el tipo de transDecs.
-- Tip2: Van a tener que pensar bien que hacen. Ver transExp (LetExp...)
-- ** transDecs :: (MemM w, Manticore w) => [Dec] -> w a -> w a
transDecs :: (Manticore w) => [Dec] -> w a -> w a
transDecs ((FunctionDec fs) : xs)          = id
transDecs ((VarDec nm escap t init p): xs) = id
transDecs ((TypeDec xs): xss)              = id

-- ** transExp :: (MemM w, Manticore w) => Exp -> w (BExp , Tipo)
transExp :: (Manticore w) => Exp -> w (() , Tipo)
transExp (VarExp v p) = addpos (transVar v) p
transExp UnitExp{} = return ((), TUnit) -- ** fmap (,TUnit) unitExp
transExp NilExp{} = return ((), TNil) -- ** fmap (,TNil) nilExp
transExp (IntExp i _) = return ((), TInt RW) -- ** fmap (,TInt RW) (intExp i)
transExp (StringExp s _) = return (() , TString) -- ** fmap (,TString) (stringExp (pack s))
transExp (CallExp nm args p) = undefined -- Completar
transExp (OpExp el' oper er' p) = do -- Esta va gratis
        (_ , el) <- transExp el'
        (_ , er) <- transExp er'
        return ( () , if (intiposIguales el er) then TInt RO else (P.error "Tipos Diferentes"))
        -- case oper of
            -- EqOp  -> do
            --         C.unless (okOp el er oper) (P.error ("Tipos no comparables " ++ show el ++ show er ++ show oper))
                    -- c <- (if intiposIguales el TString
                    --       then (binOpStrExp cl oper cr)
                    --       else (binOpIntRelExp cl oper cr)
                    --      )
                    -- error "Return TInt más el código intermedio generado"
            -- _ -> P.error "Pensar"
            -- NeqOp ->
            -- PlusOp ->
            -- MinusOp ->
            -- TimesOp ->
            -- DivideOp ->
            -- LtOp ->
            -- LeOp ->
            -- GtOp ->
            -- GeOp ->
transExp(RecordExp flds rt p) = error "Completar" -- Completar
                                -- Especial atención acá.
                                -- Tenemos una lista de expresiones con efectos
                                -- y estos efectos tiene producirse en orden!
transExp(SeqExp es p) = do -- Va gratis
        es' <- mapM transExp es
        -- c <- seqExp (map fst es')
        -- return ( c , snd $ last es')
        return ( () , snd $ last es')
transExp(AssignExp var val p) = error "Completar"
transExp(IfExp co th Nothing p) = do
        -- ** (ccond , co') <- transExp co
        (_ , co') <- transExp co
        C.unlessM (tiposIguales co' $ TInt RW) $ P.error "Error en la condición"
        -- ** (cth , th') <- transExp th
        (() , th') <- transExp th
        C.unlessM (tiposIguales th' TUnit) $ P.error "La expresión del then no es de tipo unit"
        -- ** cres <- ifThenExp ccond cth
        -- ** return (cres, TUnit)
        return (() , TUnit)
transExp(IfExp co th (Just el) p) = error "Completar" -- Completar
transExp(WhileExp co body p) = error "Completar" -- Completar
transExp(ForExp nv mb lo hi bo p) = error "Completar" -- Completar
transExp(LetExp dcs body p) = transDecs dcs (transExp body)
transExp(BreakExp p) = error "Completar" -- Va gratis ;)
transExp(ArrayExp sn cant init p) = error "Completar" -- Completar


-- Un ejemplo de estado que alcanzaría para realizar todas la funciones es:
-- data EstadoG = G {unique :: Int, vEnv :: [M.Map Symbol EnvEntry], tEnv :: [M.Map Symbol Tipo]}
--     deriving Show
--
-- Acompañado de un tipo de errores
-- data SEErrores = NotFound T.Text | DiffVal T.Text | Internal T.Text
--     deriving Show
--
--  Podemos definir el estado inicial como:
-- initConf :: EstadoG
-- initConf = G {unique = 0
--             , tEnv = [M.insert (T.pack "int") (TInt RW) (M.singleton (T.pack "string") TString)]
--             , vEnv = [M.fromList
--                     [(T.pack "print", Func (1,T.pack "print",[TString], TUnit, True))
--                     ,(T.pack "flush", Func (1,T.pack "flush",[],TUnit, True))
--                     ,(T.pack "getchar",Func (1,T.pack "getchar",[],TString,True))
--                     ,(T.pack "ord",Func (1,T.pack "ord",[TString],TInt RW,True)) -- Ojota con este intro...
--                     ,(T.pack "chr",Func (1,T.pack "chr",[TInt RW],TString,True))
--                     ,(T.pack "size",Func (1,T.pack "size",[TString],TInt RW,True))
--                     ,(T.pack "substring",Func (1,T.pack "substring",[TString,TInt RW, TInt RW],TString,True))
--                     ,(T.pack "concat",Func (1,T.pack "concat",[TString,TString],TString,True))
--                     ,(T.pack "not",Func (1,T.pack "not",[TInt RW],TInt RW,True))
--                     ,(T.pack "exit",Func (1,T.pack "exit",[TInt RW],TUnit,True))
--                     ]]}
-- Utilizando alguna especia de run de la monada definida, obtenemos algo así
-- runLion :: Exp -> Either SEErrores Tipo
-- runLion e = run (transExp e) initConf
