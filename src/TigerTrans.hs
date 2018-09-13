{-# Language UndecidableInstances  #-}
module TigerTrans where

import qualified Control.Monad.State as ST
import           Prelude             hiding (EQ, GT, LT, error, exp, seq)
import qualified Prelude             as P (error)
import qualified TigerAbs            as Abs
import           TigerErrores
import           TigerFrame          as F
import           TigerSres           (Externa (..))
import           TigerSymbol         as T
import           TigerTemp
import           TigerTree

import           Control.Monad
import qualified Data.Foldable       as Fold
import           Data.List           as List
import           Data.Ord            hiding (EQ, GT, LT)


import           Debug.Trace

-- | Reexportamos el tipo de Fragmentos provenientes de TigerTrans.
type TransFrag = Frag

-- | Tipo de datos representando si es un procedimiento o una función
data IsProc = IsProc | IsFun

-- | Empaquetadores de expresiones
-- Esto pasa ya que la información de contexto, es decir, donde están cada
-- una de las expresiones, statements y/o condicionales, lo sabemos
-- en el otro modulo, en [TigerSeman].
data BExp where
  -- | Representa una expresión. Es decir que se espera que devuelva
  -- algún resultado.
  Ex :: Exp -> BExp
  -- | Representan las computaciones que no dan resultados, es decir
  -- un /statement/
  Nx :: Stm -> BExp
  -- | Representan a expresiones que representan condicionales.
  Cx  :: ( -- | Dadas las etiquetas a donde saltar en caso de verdadero
           -- o falso.
          (Label, Label)
         -- | Y devolvemos un Statement formado correctamente.
          -> Stm)
      -> BExp

instance Show BExp where
    show (Ex e)  = "Ex " ++ show e
    show (Nx e)  = "Nx " ++ show e
    show (Cx _ ) = "Cx "

-- | Función helper /seq/ que nos permite escribir
-- fácilmente una secuencia de [Stm] usando listas.
seq :: [Stm] -> Stm
seq []     = ExpS $ Const 0
seq [s]    = s
seq (x:xs) = Seq x (seq xs)

-- | Eventualmente vamos a querer obtener nuevamente las expresiones
-- empaquetas por este nuevo tipo [BExp]. Para eso damos las siguientes
-- funciones des-empaquetadoras. Definidas en [7.3] del libro.

-- | Des-empaquetador de expresiones
-- Es mónadico ya que deberá crear labels, y temps
-- para des-empaquetar una condición.
unEx :: (Monad w, TLGenerator w) => BExp -> w Exp
unEx (Ex e) = return e
unEx (Nx s) = return $ Eseq s (Const 0)
unEx (Cx cf) = do
    r <- newTemp
    t <- newLabel
    f <- newLabel
    return $ Eseq
        (seq [
            Move (Temp r) (Const 1),
            cf(t,f),
            Label f,
            Move (Temp r) (Const 0),
            Label t])
        (Temp r)


-- | Des-empaquetador de statements
unNx :: (Monad w,TLGenerator w) => BExp -> w Stm
unNx (Ex e) = return $ ExpS e
unNx (Nx s) = return s
unNx (Cx cf) = do
        t <- newLabel
        return $ seq [cf(t,t),Label t]

-- | Des-empaquetador de condiciones
unCx :: (Monad w,TLGenerator w, Demon w) => BExp -> w ((Label, Label) -> Stm)
unCx (Nx s)         = internal $ pack "unCx(Nx...)"
unCx (Cx cf)        = return cf
-- Pequeña optimización boluda
unCx (Ex (Const 0)) = return (\(_,f) -> Jump (Name f) f)
unCx (Ex (Const _)) = return (\(t,_) -> Jump (Name t) t)
unCx (Ex e)         = return (uncurry (CJump NE e (Const 0)))

-- | Los niveles son un stack de (Frame, Int)
-- Recordar que Frame es una representación del Marco Virtual.
data LevelI = MkLI {getFrame' :: Frame, getNlvl' :: Int}
  deriving Show

type Level = [LevelI]

-- | Helpers de niveles.
getFrame :: Level -> Frame
getFrame = getFrame' . head

getNlvl :: Level -> Int
getNlvl = getNlvl' . head

setFrame :: Frame -> Level -> Level
setFrame f (MkLI _ l : xs) = MkLI f l : xs
setFrame _ _               = P.error "setFrame"

newLevel :: Level -> Symbol -> [Bool] -> Level
newLevel [] s bs                 = [MkLI (newFrame s bs) 0]
newLevel ls@(MkLI _ lvl :_) s bs = MkLI (newFrame s bs) (lvl+1) : ls

getParent :: Level -> Level
getParent []     = P.error "No fuimos del outermost level"
getParent (_:xs) = xs

outermost :: Level
outermost = [ MkLI (newFrame (pack "_undermain") []) (-1) ]

-- | Clase encargada del manejo de memoria y niveles.
-- Esta etapa va a consumir el AST y construir un nuevo lenguaje llamado Código
-- Intermedio. En este proceso vamos tomando nota cuantas variables define una
-- función o let, para eventualmente crear los marcos necesarios para le
-- ejecución de código assembler.
class (Monad w, TLGenerator w, Demon w) => MemM w where
    -- | Level management
    -- Es un entero que nos indica en qué nivel estamos actualmente.
    getActualLevel :: w Int
    upLvl :: w ()
    downLvl :: w ()
    -- | Salida management.
    -- Esta etiqueta la necesitamos porque es la que nos va permitir saltar a la
    -- salida de un while (Ver código intermedio de While). Usada en el break.
    pushSalida :: Maybe Label -> w ()
    topSalida :: w (Maybe Label)
    popSalida :: w ()
    -- | Level management Cont. El nivel en esta etapa es lo que llamamos el
    -- marco de activación virtual o dinámico (no me acuerdo). Pero es lo que
    -- eventualmente va a ser el marco de activación
    pushLevel :: Level -> w ()
    popLevel  :: w ()
    topLevel  :: w Level
    -- | Manejo de /pedido/ de memoria para variables locales.
    -- Esto básicamente debería aumentar en uno la cantidad de variables locales
    -- usadas. Es lo que se usará eventualmente para toquetear el stack o lo que
    -- sea que use la arquitectura deseada.
    allocLocal :: Bool -> w Access
    allocLocal b = do
      -- | Dame el nivel actual
        t <- topLevel
        popLevel
      -- dame una versión modificada según lo dicte
      -- el módulo de manejo de Frame (que simula la arquitectura)
        (f,acc) <- F.allocLocal (getFrame t) b
      -- este nuevo frame es lo que vamos a usar.
        let nt = setFrame f t
        pushLevel nt
      -- y devolvemos el acceso creado. Si está en un temporal (registro) o en
      -- memoria (y en qué /offset/ del /fp/).
        return  acc
    -- | Manejo de /pedido/ de memoria para argumentos.
    -- ver lo que hicimos en /allocLocal/
    allocArg :: Bool -> w Access
    allocArg b = do
        t <- topLevel
        popLevel
        (f,a) <- F.allocArg (getFrame t) b
        pushLevel (setFrame f t)
        return a
    -- | Frag management
    -- Básicamente los fragmentos van a ser un efecto lateral de la computación.
    -- Recuerden que los fragmentos son pedazos de código intermedio que se van
    -- a ejecutar. Y estos son un efecto lateral porque todavía no sabemos bien
    -- cómo van a ser ejecutados (eso se decide más adelante)
    pushFrag  :: Frag -> w ()
    getFrags  :: w [Frag]

-- | Generación de código intermedio.
-- Cada construcción del (AST)[src/TigerAbs.hs] la consumiremos
-- y construiremos un fragmento de código intermedio que eventualmente
--  se traducirá en código de máquina y ejecutará.
-- Algunas funciones se especializan más para conseguir un mejor código intermedio.
class IrGen w where
    -- | Esta función mágica prepara la máquina para comenzar a traducir una función o procedimiento.
    -- básicamente es la que va a agregar el Fragmento que es generado por la
    -- función y ponerlo como el efecto secundario mencionado más arriba
    procEntryExit :: Level -> BExp -> w ()
    unitExp :: w BExp
    nilExp :: w BExp
    intExp :: Int -> w BExp
    stringExp :: Symbol -> w BExp
    simpleVar :: Access -> Int -> w BExp
    varDec :: Access -> w BExp
    fieldVar :: BExp -> Int -> w BExp
    subscriptVar :: BExp -> BExp -> w BExp
    recordExp :: [(BExp,Int)]  -> w BExp
    callExp :: Label -> Externa -> IsProc -> Level -> [BExp] -> w BExp
    letExp :: [BExp] -> BExp -> w BExp
    breakExp :: w BExp
    seqExp :: [BExp] -> w BExp
    preWhileforExp :: w ()
    posWhileforExp :: w ()
    whileExp :: BExp -> BExp -> w BExp
    forExp :: BExp -> BExp -> BExp -> BExp -> w BExp
    ifThenExp :: BExp -> BExp -> w BExp
    ifThenElseExp :: BExp -> BExp -> BExp -> w BExp
    ifThenElseExpUnit :: BExp -> BExp -> BExp -> w BExp
    assignExp :: BExp -> BExp -> w BExp
    -- preFunctionDec :: Level -> w ()
    -- posFunctionDec :: w ()
    -- Esto fuerza a que haya menos opciones... ver bien con los que lleguen a
    -- este lugar..
    envFunctionDec :: Level -> w BExp -> w BExp
    functionDec :: BExp -> Level -> IsProc -> w BExp
    binOpIntExp :: BExp -> Abs.Oper -> BExp -> w BExp
    binOpIntRelExp :: BExp -> Abs.Oper -> BExp -> w BExp
    binOpStrExp :: BExp -> Abs.Oper -> BExp -> w BExp
    arrayExp :: BExp -> BExp -> w BExp

instance (MemM w) => IrGen w where
    procEntryExit lvl bd =  do
        bd' <- unNx bd
        let res = Proc bd' (getFrame lvl)
        pushFrag res
    stringExp t = do
        l <- newLabel
        let ln = T.append (pack ".long ")  (pack $ show $ T.length t)
        let str = T.append (T.append (pack ".string \"") t) (pack "\"")
        pushFrag $ AString l [ln,str]
        return $ Ex $ Name l
    -- | Función utilizada para la declaración de una función.
    envFunctionDec lvl funDec = do
        -- preFunctionDec
        -- mandamos un nada al stack, por si un /break/ aparece en algún lado que
        -- no tenga un while y detectamos el error. Ver [breakExp]
        pushSalida Nothing
        upLvl
        pushLevel lvl
        fun <- funDec
        -- posFunctionDec
        -- | Cuando salimos de la función sacamos el 'Nothing' que agregamos en 'preFunctionDec'.
        popSalida
        downLvl
        -- devolvemos el código en el entorno donde fue computada.
        return fun
    -- functionDec :: BExp -> Level -> Bool -> w BExp
    functionDec bd lvl proc = do
        body <- case proc of
                  IsProc -> unNx bd
                  IsFun  -> Move (Temp rv) <$> unEx bd
        procEntryExit lvl (Nx body)
        return $ Ex $ Const 0
    simpleVar acc level = P.error "COMPLETAR"
    varDec acc = do { i <- getActualLevel; simpleVar acc i}
    unitExp = return $ Ex (Const 0)
    nilExp = return $ Ex (Const 0)
    intExp i = return $ Ex (Const i)
    fieldVar be i = P.error "COMPLETAR"
    -- subscriptVar :: BExp -> BExp -> w BExp
    subscriptVar var ind = do
        evar <- unEx var
        eind <- unEx ind
        tvar <- newTemp
        tind <- newTemp
        return $ Ex $
            Eseq
                (seq    [Move (Temp tvar) evar
                        ,Move (Temp tind) eind
                        ,ExpS $ externalCall "_checkIndex" [Temp tvar, Temp tind]])
                (Mem $ Binop Plus (Temp tvar) (Binop Mul (Temp tind) (Const wSz)))
    -- recordExp :: [(BExp,Int)]  -> w BExp
    recordExp flds = P.error "COMPLETAR"
    -- callExp :: Label -> Externa -> Bool -> Level -> [BExp] -> w BExp
    callExp name external isproc lvl args = P.error "COMPLETAR"
    -- letExp :: [BExp] -> BExp -> w BExp
    letExp [] e = do -- Puede parecer al dope, pero no...
            e' <- unEx e
            return $ Ex e'
    letExp bs body = do
        bes <- mapM unNx bs
        be <- unEx body
        return $ Ex $ Eseq (seq bes) be
    -- breakExp :: w BExp
    -- | JA! No está implementado
    breakExp = P.error "COMPLETAR"
    -- seqExp :: [BExp] -> w BExp
    seqExp [] = return $ Nx $ ExpS $ Const 0
    seqExp bes = do
        let ret = last bes
        case ret of
            Nx _ -> Nx . seq <$> mapM unNx bes
            Ex e' -> do
                    let bfront = init bes
                    ess <- mapM unNx bfront
                    return $ Ex $ Eseq (seq ess) e'
            _ -> internal $ pack "WAT!123"
    -- preWhileforExp :: w ()
    preWhileforExp = newLabel >>= pushSalida . Just
    -- posWhileforExp :: w ()
    posWhileforExp = popSalida
    -- whileExp :: BExp -> BExp -> Level -> w BExp
    -- | While Loop.
    -- ```
    --   test:
    --        if (condition) goto body else done
    --        body:
    --             body (Un break acá se traduce como un salto a done)
    --        goto test
    --   done:
    -- ```
    whileExp cond body = do
        -- | Desempaquetamos la condición como un condicional
        ccond <- unCx cond
        -- | Desempaquetamos el body como un statement
        cbody <- unNx body
        -- | Creamos dos etiquetas para los saltos del if
        -- una correspondiente al test
        test <- newLabel
        -- | otra correspondiente al cuerpo
        body <- newLabel
        -- | buscamos en el stackla etiqueta de salida (done).
        lastM <- topSalida
        case lastM of
            Just done ->
                return $ Nx $ seq
                    [Label test
                    , ccond (body,done)
                    , Label body
                    , cbody
                    , Jump (Name test) test
                    , Label done]
            _ -> internal $ pack "no label in salida"
    -- forExp :: BExp -> BExp -> BExp -> BExp -> w BExp
    forExp lo hi var body = P.error "COMPLETAR"
    -- ifThenExp :: BExp -> BExp -> w BExp
    ifThenExp cond bod = P.error "COMPLETAR"
    -- ifThenElseExp :: BExp -> BExp -> BExp -> w BExp
    ifThenElseExp cond bod els = P.error "COMPLETAR"
    -- ifThenElseExpUnit :: BExp -> BExp -> BExp -> w BExp
    ifThenElseExpUnit _ _ _ = P.error "COmpletaR?"
    -- assignExp :: BExp -> BExp -> w BExp
    assignExp cvar cinit = do
        cvara <- unEx cvar
        cin <- unEx cinit
        case cvara of
            Mem v' ->  do
                t <- newTemp
                return $ Nx $ seq [Move (Temp t) cin, Move cvara (Temp t)]
            _ -> return $ Nx $ Move cvara cin
    -- binOpIntExp :: BExp -> Abs.Oper -> BExp -> w BExp
    binOpIntExp le op re = P.error "COMPLETAR"
    -- binOpStrExp :: BExp -> Abs.Oper -> BExp -> w BExp
    binOpStrExp strl op strr = P.error "COMPLETAR"
    binOpIntRelExp op strr = P.error "COMPLETAR"
    -- arrayExp :: BExp -> BExp -> w BExp
    arrayExp size init = do
        sz <- unEx size
        ini <- unEx init
        t <- newTemp
        return $ Ex $ Eseq (seq
                [ExpS $ externalCall "_allocArray" [sz,ini]
                , Move (Temp t) (Temp rv)
                ]) (Temp t)
