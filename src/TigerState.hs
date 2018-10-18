{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# Language UndecidableInstances  #-}

module TigerState where
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

-- Estructura de datos que se encarga de llevar el estado
data Estado = 
    ST {
            unique :: Integer, 
            vEnv :: M.Map Symbol EnvEntry, 
            tEnv :: M.Map Symbol Tipo, 
            level :: Level,
            actualLevel :: Int,
            salidas :: [Maybe Label],
            frags :: [Frag]
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
    show estado = "Entorno de tipos: \n\n" ++ (mostrarEnv $ tEnv estado) ++ "\n" ++
                  "Entorno de variables: \n\n" ++ (mostrarEnv $ vEnv estado) ++ "\n" ++
                  "Level: \n\n" ++ show (level estado)

-- Tipo que modela los errores
data Errores = NotFound T.Text | DiffVal T.Text | Internal T.Text
    deriving Show

-- Configuracion inicial del compilador.
initConf :: Estado
initConf = 
    ST  {
            unique = 0,
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
            level = newLevel outermost (pack "main") [True], --Por ahora sin argumentos, true por el static link a outer.
            -- actualLevel se refiere al scope que estamos viendo en la variable actual,
            -- lo podemos usar para navegar los lvls.
            actualLevel = 0,
            salidas = [],
            frags = []
        }

type TigerState = StateT Estado (Either [Errores])

   

instance Demon TigerState where
    --derror :: Symbol -> w a
    derror s = lift (Left [(Internal s)])
    --adder :: w a -> Symbol -> w a
    adder m s = do estado <- get
                   case (runStateT m) estado of
                        Left err -> lift (Left ((Internal s):err))
                        Right _ -> lift (Left [Internal s])
--Función auxiliar, similar a withState, solo que repone el estado anterior al terminar.
withStateOnce :: Monad m => (s -> s) -> StateT s m a -> StateT s m a
withStateOnce f m = do sta <- get
                       put (f sta)
                       a <- m
                       put sta
                       return a
                        
--Implementacion de manticore
instance Manticore TigerState where
    insertValV simbolo entrada = withStateOnce (insertValV' simbolo entrada) 
    insertFunV simbolo entrada = withStateOnce (insertFunV' simbolo entrada)
    insertVRO s = withStateOnce (insertVRO' s)
    insertTipoT s t = withStateOnce  (insertTipoT' s t)
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
                 return $ T.pack ("Temp" ++ show u)
    newLabel = do estado <- get
                  let u = unique estado
                  put estado{unique = u + 1}
                  return $ T.pack ("Label" ++ show u)
 
--Funciones auxiliares para implementar manticore                            
insertValV' :: Symbol -> ValEntry -> Estado -> Estado
insertValV' simbolo entrada estado = 
    estado
    {
        vEnv = insert simbolo (Var entrada)  (vEnv estado)
    }
insertFunV' :: Symbol -> FunEntry -> Estado -> Estado
insertFunV' simbolo entrada estado = 
    estado
    {
        vEnv = insert simbolo (Func entrada)  (vEnv estado)
    }
insertVRO' :: Symbol -> Estado -> Estado
insertVRO' simbolo estado = estado --TODO
insertTipoT' :: Symbol -> Tipo -> Estado -> Estado
insertTipoT' simbolo tipo estado = 
    estado
    {
        tEnv = insert simbolo tipo (tEnv estado)
    }
getTipoFunV' :: Symbol -> Estado -> Maybe FunEntry
getTipoFunV' s  estado =  case (M.lookup s (vEnv estado)) of
                                Nothing -> Nothing
                                Just (Var e) -> Nothing
                                Just (Func f) -> Just f
getTipoValV' :: Symbol -> Estado -> Maybe ValEntry
getTipoValV' s  estado = case (M.lookup s (vEnv estado)) of
                                Nothing -> Nothing
                                Just (Var e) -> Just e
                                Just (Func f) -> Nothing
getTipoT' :: Symbol -> Estado -> Maybe Tipo
getTipoT' s e = (M.lookup s (tEnv e))


 
--TODO: Este código está mal, creo que upLvl debería mover el actualLevel nomás, y el resto
instance MemM TigerState where 
    getActualLevel = do estado <- get
                        return $ actualLevel estado
    upLvl = do estado <- get
               let niveles = level estado
               let actual = actualLevel estado
               let (MkLI frame lastLevel) = L.head niveles
               if(actual == lastLevel)
                then internal $ T.pack "Nivel mas alto"
                else put estado{actualLevel = actual + 1}
    downLvl = do estado <- get
                 let niveles = level estado
                 let actual = actualLevel estado
                 let (MkLI frame lastLevel) = L.last niveles
                 if(actual == -1)
                  then internal $ T.pack "Nivel mas bajo"
                  else put estado{actualLevel = actual - 1}
    pushSalida s = do
        estado <- get
        let ss = salidas estado
        put estado{salidas = (s:ss)}
    topSalida = do
        estado <- get
        let sas = salidas estado
        case sas of
            [] -> internal $ T.pack "No hay salidas"
            (s:sas) -> return s
    popSalida = do
        estado <- get
        let sas = salidas estado
        case sas of
            [] -> internal $ T.pack "No hay salidas para sacar"
            (s:ss) -> put estado{salidas = ss}
    pushLevel ((MkLI pf0 lala):ls) = do estado <- get
                                        let ((MkLI pf  lvl):ls) = level estado
                                        put estado {
                                            level = ((MkLI pf0  (lvl+1)):(MkLI pf lvl):ls), 
                                            actualLevel = lvl + 1
                                        }
    popLevel = do estado <- get
                  let (l:ls) = level estado
                  put estado{level = ls}
    topLevel = do estado <- get
                  let (l:ls) = level estado
                  return [l]
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

unicaPosicion :: Pos
unicaPosicion = Simple {line = 0, col = 0}

programaPrueba :: Exp
programaPrueba = 
    LetExp 
      [
        TypeDec [(T.pack "enteros", NameTy $ T.pack "int", unicaPosicion),
                 (T.pack "elRecord", RecordTy [(T.pack "a", NameTy $ T.pack "enteros")], unicaPosicion),
                 (T.pack "elOtroRecord", RecordTy [(T.pack "b", NameTy $ T.pack "elRecord")], unicaPosicion),
                 (T.pack "elReOtroRecord", RecordTy [(T.pack "b", NameTy $ T.pack "elOtroRecord")], unicaPosicion)
                 ]
      ]
      (SeqExp [] unicaPosicion)
      unicaPosicion
  
run :: Exp -> Estado -> Either [Errores] Estado
run expre estadoInicial = execStateT (transExp expre :: TigerState (BExp, Tipo)) estadoInicial
  
  
  
showVEnv env = putStrLn $ show (vEnv env)
showTEnv env = putStrLn $ show (tEnv env)