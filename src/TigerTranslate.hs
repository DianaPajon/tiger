{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances  #-}

--Implementación completa de la capa Translate.

module TigerTranslate where
import           TigerSeman
import           TigerTips
import           TigerAbs
import           TigerTrans
import           TigerTemp
import           TigerSymbol
import           TigerSres
import           TigerErrores
import           TigerFrame
import           TigerUnique
import           Data.Text as T
import           Data.Map as M
import           Control.Monad.State
import           GHC.List as L
import           TigerTree as Tree
import           TigerPrettyIr
import           TigerEmit
-- Estructura de datos que se encarga de llevar el estado
data Estado = 
    ST {
            scope :: Scope,
            unique :: Integer, 
            level :: Level,
            actualLevel :: Int,
            frags :: [Frag]
    }

data Scope = 
    SC {
        vEnv :: M.Map Symbol EnvEntry, 
        tEnv :: M.Map Symbol Tipo, 
        salidas :: [Maybe Label]
    }
-- Función para mostrar el entorno
mostrarEnv :: Show a => (M.Map Symbol a) -> String
mostrarEnv e = "{" ++
                    Prelude.foldr (\s v -> case s of 
                                    [] -> "\n   " ++ v
                                    _ -> "\n   " ++ s ++ ", " ++ v
                            )
                            ""  
                            (L.map (\(k, v) -> "[" ++ show k ++ ", " ++ show v ++ "]") $ toList e)
                    ++ "\n}"   
       

instance Show Estado where
    show estado = "\nEntorno de tipos: \n\n" ++ (mostrarEnv $ tEnv $ scope estado) ++ "\n" ++
                  "Entorno de variables: \n\n" ++ (mostrarEnv $ vEnv $ scope estado) ++ "\n" ++
                  "Level: \n\n" ++ show (level estado) ++ "\n" ++ 
                  "Frags: \n\n" ++ show (frags estado)


-- Configuracion inicial del compilador.
initConf :: Estado
initConf = 
    ST  {
            scope = SC {
                tEnv = M.insert (T.pack "int") (TInt RW) (M.singleton (T.pack "string") TString),
                vEnv = M.fromList
                        [(pack "print", Func (outermost,pack "print",[TString], TUnit, Runtime)),
                        (pack "flush", Func (outermost,pack "flush",[],TUnit, Runtime)),
                        (pack "getchar",Func (outermost,pack "getchar",[],TString,Runtime)),
                        (pack "ord",Func (outermost,pack "ord",[TString],TInt RW,Runtime)),
                        (pack "chr",Func (outermost,pack "chr",[TInt RW],TString,Runtime)),
                        (pack "size",Func (outermost,pack "size",[TString],TInt RW,Runtime)),
                        (pack "substring",Func (outermost,pack "substring",[TString,TInt RW, TInt RW],TString,Runtime)),
                        (pack "concat",Func (outermost,pack "concat",[TString,TString],TString,Runtime)),
                        (pack "not",Func (outermost,pack "not",[TBool],TBool,Runtime)),
                        (pack "exit",Func (outermost,pack "exit",[TInt RW],TUnit,Runtime))
                        ]
                    ,
                -- level indica el nivel de anidamiento a donde llega el código, es decir, la funcion que estamos compilando.
                salidas = []
            
            },
            level = newLevel outermost (pack "_tigermain") [], --Por ahora sin argumentos, true por el static link a outer.
            -- actualLevel se refiere al scope que estamos viendo en la variable actual,
            -- lo podemos usar para navegar los lvls.
            actualLevel = 0,
            unique = 0,
            frags = []
        }

type TigerState = StateT Estado (Either [Errores])

instance Demon TigerState where
    --derror :: Symbol -> w a
    derror s = lift (Left [(Error s)])
    --adder :: w a -> Symbol -> w a
    adder m s = do estado <- get
                   case (runStateT m) estado of
                        Left err -> lift $ Left ((Error s):err)
                        Right x -> do put (snd x) --TODO: Esto tipa porque asi lo armé, pero no lo entiendo, revisar.
                                      return (fst x)
--Función auxiliar, similar a withState, solo que repone el estado anterior al terminar.
changeScope :: Monad m => (Scope -> Scope) -> StateT Estado m a -> StateT Estado m a
changeScope f m = do sta <- get
                     --Aplico el nuevo scope
                     put (sta{scope = f $ scope sta})
                     a <- m
                     nuevoEstado <- get
                     put (nuevoEstado{scope = scope sta})
                     return a
                
--Implementacion de manticore
instance Manticore TigerState where
    insertValV simbolo entrada = changeScope (insertValV' simbolo entrada) 
    insertFunV simbolo entrada = changeScope (insertFunV' simbolo entrada)
    insertVRO s = changeScope (insertVRO' s)
    insertTipoT s t = changeScope  (insertTipoT' s t)
    getTipoFunV s = do estado <- get
                       case getTipoFunV' s estado of 
                            Just e -> return e
                            Nothing -> derror $ T.pack $ "Funcion no encontrada" ++  (show s)
    getTipoValV s = do estado <- get
                       case getTipoValV' s estado of 
                            Just e -> return e
                            Nothing -> derror $ T.pack $ "Val no encontrada" ++ (show s)
    getTipoT s = do estado <- get
                    case getTipoT' s estado of 
                        Just t -> return t
                        Nothing -> derror $ T.pack $ "Tipo no encontrado" ++ (show s)
    showVEnv  = undefined
    showTEnv  = undefined
    ugen  = do estado <- get
               let u = unique estado
               let estado' = estado{unique = u + 1}
               put estado'
               return u

instance UniqueGenerator TigerState where
    mkUnique = ugen

instance TLGenerator TigerState where
    newTemp = do estado <- get
                 let u = unique estado
                 put estado{unique = u + 1}
                 return $ T.pack ("t" ++ show u)
    newLabel = do estado <- get
                  let u = unique estado
                  put estado{unique = u + 1}
                  return $ T.pack ("L" ++ show u)
 
--Funciones auxiliares para implementar manticore                            
insertValV' :: Symbol -> ValEntry -> Scope -> Scope
insertValV' simbolo entrada estado = 
    estado
    {
        vEnv = insert simbolo (Var entrada)  $! (vEnv estado)
    }
insertFunV' :: Symbol -> FunEntry -> Scope -> Scope
insertFunV' simbolo entrada estado = 
    estado
    {
        vEnv = insert simbolo (Func entrada)  $! (vEnv estado)
    }
insertVRO' :: Symbol -> Scope -> Scope
insertVRO' simbolo estado = estado --TODO
insertTipoT' :: Symbol -> Tipo -> Scope -> Scope
insertTipoT' simbolo tipo estado = 
    estado
    {
        tEnv = insert simbolo tipo (tEnv estado)
    }
getTipoFunV' :: Symbol -> Estado -> Maybe FunEntry
getTipoFunV' s  estado =  case (M.lookup s (vEnv $ scope estado)) of
                                Nothing -> Nothing
                                Just (Var e) -> Nothing
                                Just (Func f) -> Just f
getTipoValV' :: Symbol -> Estado -> Maybe ValEntry
getTipoValV' s  estado = case (M.lookup s (vEnv $ scope estado)) of
                                Nothing -> Nothing
                                Just (Var e) -> Just e
                                Just (Func f) -> Nothing
getTipoT' :: Symbol -> Estado -> Maybe Tipo
getTipoT' s e = (M.lookup s (tEnv $ scope e))


 
--TODO: Este código está mal, creo que upLvl debería mover el actualLevel nomás, y el resto
instance MemM TigerState where 
    getActualLevel = do estado <- get
                        let actualLevel = getNlvl $ level  estado
                        return actualLevel
    pushSalida s = do
        estado <- get
        let ss = salidas $ scope estado
        put estado{scope = (scope estado){salidas = (s:ss)}}
    topSalida = do
        estado <- get
        let sas = salidas $ scope estado
        case sas of
            [] -> internal $ T.pack "No hay salidas"
            (s:sas) -> return s
    popSalida = do
        estado <- get
        let sas = salidas $ scope estado
        case sas of
            [] -> internal $ T.pack "No hay salidas para sacar"
            (s:ss) -> put estado{scope = (scope estado){salidas = ss}}
    pushLevel [] = internal $ T.pack "Se quiere agregar un lvl vacío"
    pushLevel lvl = do estado <- get --"Push", ponele. En la práctica va a ser un push. "Push" es un Frame, no un lvl.
                       put estado{ 
                        level = lvl,
                        actualLevel = getNlvl lvl
                       }
    popLevel = do estado <- get
                  let (l:ls) = level estado
                  put estado{level = ls}
    topLevel = do estado <- get
                  return $ level estado
    pushFrag f = do
        estado <- get
        let fs = frags estado
        put estado{frags = f:fs}
    getFrags = do
        estado <- get
        return $ frags estado


upLvl' :: Level -> Int -> Maybe Int
upLvl' (( MkLI f l):(MkLI f' l'):ls) i = if l == i then Just l' else upLvl' ((MkLI f' l'):ls) i
upLvl' ((MkLI f l):[]) i = if  l == i then Just l else Nothing
upLvl' [] i = Nothing

downLvl' :: Level -> Int -> Maybe Int
downLvl' ((MkLI f l):(MkLI f' l'):ls) i = if l' == i then Just l else downLvl' ((MkLI f' l'):ls) i
downLvl' ((MkLI f l):[]) i = if l == i then Just l else Nothing
downLvl' [] i = Nothing


