{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances  #-}

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
import           TigerCanon
import           TigerTree as Tree
import           TigerPrettyIr
import           TigerEmit
-- Estructura de datos que se encarga de llevar el estado
data Estado = 
    ST {
            unique :: Integer, 
            vEnv :: M.Map Symbol EnvEntry, 
            tEnv :: M.Map Symbol Tipo, 
            level :: Level,
            actualLevel :: Int,
            salidas :: [Maybe Label],
            frags :: [Frag],
            assembly :: [Assem]
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
data Errores = NotFound T.Text | DiffVal T.Text | Internal T.Text | Error T.Text
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
            frags = [],
            assembly = []
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
        vEnv = insert simbolo (Var entrada)  $! (vEnv estado)
    }
insertFunV' :: Symbol -> FunEntry -> Estado -> Estado
insertFunV' simbolo entrada estado = 
    estado
    {
        vEnv = insert simbolo (Func entrada)  $! (vEnv estado)
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
{-    upLvl = do estado <- get
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
-}
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
    pushLevel [] = internal $ T.pack "Se quiere agregar un lvl vacío"
    pushLevel lvl = do estado <- get --"Push", ponele. En la práctica va a ser un push. "Push" es un Frame, no un lvl.
                       put estado {
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

instance Emisor TigerState where
    emit instr = do
        estado <- get
        let instrucciones = assembly estado
        put estado{assembly = instrucciones ++ [instr]}
    getCode = do
        estado <- get
        return $ assembly estado
    munchExp reg@(Temp t) = do
        let assem = Oper {
            oassem = "",
            osrc = [],
            odest = [],
            ojump = Nothing
        }
        return t
--Caso particular, Mem usado en el lado izquierdo de un move.--FODO: Registrer addessing con offset.
    munchStm (Move (Mem e1) e2) = do
        t1 <- munchExp e1
        t2 <- munchExp e2
        emit Oper {
            oassem = "movl `s0, (`d0)"
           ,osrc = [t2]
           ,odest = [t1]
           ,ojump = Nothing
        }
--Otro caso particular, un label el el lado izquierdo de un move.
    munchStm (Move (Name l1) e2) = do
        t2 <- munchExp e2
        let label = unpack l1
        emit Oper {
            oassem = "movl `s0, (label)"
           ,osrc = [t2]
           ,odest = []
           ,ojump = Nothing
        }
--Caso general e1 puede ser un registro solamente, es el caso "Mover a un registro". Obvia optimizacion usar un solo registro.
    munchStm (Move e1 e2) = do
        t2 <- munchExp e2
        t1 <- munchExp e1
        emit Oper {
            oassem = "movl `s0, `d0"
           ,osrc = [t2]
           ,odest = [t1]
           ,ojump = Nothing
        }
    munchStm (Jump (Name l1) l2) = do
        let label = unpack l1
        emit Oper {
            oassem = "jmp " ++ label
           ,osrc = []
           ,odest = []
           ,ojump = Just [l1]
        }
    munchStm (CJump op e1 e2 tl fl) = do
        t1 <- munchExp e1
        t2 <- munchExp e2
        let trueDest = unpack tl
        let falseDest = unpack fl
        --Emito 3 instrucciones. Comparo primero
        emit Oper {
            oassem = "cmp `s0, `s1" --Revisar si el canonizado no simplifica esto
           ,osrc = [t1,t2]
           ,odest = []
           ,ojump = Nothing
        }
        --Salto condicional al target true
        emit Oper {
            oassem = jumpInstruction op ++ " " ++ trueDest
           ,osrc = []
           ,odest = []
           ,ojump = Just [tl]
        }
        --En caso de que no pase nada, salto al target falso.
        emit Oper {
            oassem = "jmp " ++ falseDest
           ,osrc = []
           ,odest = []
           ,ojump = Just [fl]
        }
    --Instrucciones de salto condicional de x86 según el operador.
      where jumpInstruction op = case op of
                                    Tree.EQ -> "je"
                                    Tree.NE -> "jne"
                                    Tree.LT -> "jl"
                                    Tree.GT -> "jg"
                                    Tree.LE -> "jle"
                                    Tree.GE -> "jge"
--Casos simples
    munchStm (Label l) = do
        let ls = unpack l
        let ins = ls ++ ":\n"
        emit Lab {
            lassem = ins,
            label = l
        }
    munchStm (ExpS e) = do
        munchExp e
        return ()
    munchStm (Seq s1 s2) = do
        munchStm s1
        munchStm s2
        return ()
--BEGIN CODIGO PRUEBA
unicaPosicion :: Pos
unicaPosicion = Simple {line = 0, col = 0}

programaPrueba :: TigerAbs.Exp
programaPrueba = 
    LetExp 
      [
        TypeDec [(T.pack "enteros", NameTy $ T.pack "int", unicaPosicion)],
        VarDec (T.pack "variableEntera") Escapa (Just $ T.pack "enteros") (IntExp 2 unicaPosicion) unicaPosicion,
        VarDec (T.pack "variableEntera2") Escapa (Just $ T.pack "enteros") (IntExp 4 unicaPosicion) unicaPosicion
      ]
      (StringExp "hola" unicaPosicion)
      unicaPosicion
  
runTranslate :: TigerAbs.Exp -> Either [Errores] ((BExp, Tipo),Estado)
runTranslate expre = runStateT (transExp expre :: TigerState (BExp, Tipo))  initConf



getStm :: TigerAbs.Exp -> Tree.Stm
getStm programa = 
    let 
        bexp = either (\s -> undefined) (\((a,b),c) -> a ) $ runTranslate programa
        statement = either (\s -> undefined) (\(a,c) -> a ) $ prSt
        prSt = runStateT (unNx bexp :: TigerState Tree.Stm) initConf
    in  statement


getExp :: TigerAbs.Exp -> Tree.Exp
getExp programa = 
        let 
            bexp = either (\s -> undefined) (\((a,b),c) -> a ) $ runTranslate programa
            expresion = either (\s -> undefined) (\(a,c) -> a ) $ prEx
            prEx = runStateT (unEx bexp :: TigerState Tree.Exp) initConf
        in  expresion
    
    

prettyTranslate programa = 
    let 
        bexp = either (\s -> undefined) (\((a,b),c) -> a ) $ runTranslate programa
    in renderBIr bexp

showVEnv env = putStrLn $ show (vEnv env)
showTEnv env = putStrLn $ show (tEnv env)
--END CODIGO PRUEBA
