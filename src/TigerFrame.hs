module TigerFrame where

import           TigerTemp
import           TigerTree
import           TigerAssem
import           TigerSymbol

import           Prelude     hiding (exp)

--Implementación modificada para dar los detalles de IA-32.
{-
Estructura del Frame
arg n <-------------fp + 8 + wSz * nArg
...
arg 2 <-------------fp + 8 + wSz * nArg
arg 1 <-------------fp + 8 + wSz * nArg
static link arg <---fp + 8
ret addr <----------fp + 4
ebp caller <--------fp
ebx <---------------fp - 4
esi <---------------fp - 8
edi <---------------fp - 12
local 1 <-----------fp - 12 - wSz*nLocal
local 2
local 3
...
local n <-----------fp - 12 - wSz*(cant locales)
temp 1 <------------fp - 12 - wSz*(cant locales)
...
temp n <------------fp - 12 wSz*(cant locales + cant temporales) = sp
-}

auxexp :: Int -> Exp
auxexp 0 = Temp fp
auxexp n 
 | n >= 0 = Mem(Binop Plus (auxexp (n-1)) (Const fpPrevLev))
 | otherwise = error ("Se llama a auxExp con n negativo: " ++ show n)

exp :: Access -> Int -> Exp
exp (InFrame k) e = Mem(Binop Plus (auxexp e) (Const k))
exp (InReg l) _   = Temp l



-- | Return value
rv :: Temp
rv = eax

-- | Word size in bytes
wSz :: Int
wSz = 4
-- | Base two logarithm of word size in bytes
log2WSz :: Int
log2WSz = 2

-- | Offset
fpPrev :: Int
fpPrev = 0
-- | Offset
fpPrevLev :: Int
fpPrevLev = 8

argsGap = 12
localsGap = -16

argsInicial = 0
regInicial = 0
localsInicial = 0

data Access = InFrame Int | InReg Temp | InTemp Int
    deriving Show
data Frag = Proc Stm Frame | AString Label [Symbol]

sepFrag :: [Frag] -> ([Frag],[(Stm,Frame)])
sepFrag xs = (reverse ass, reverse stmss)
    where
        (ass, stmss) = foldl (\ (lbls,stms) x ->
            case x of
                Proc st fr -> (lbls, (st,fr) : stms)
                AString {} -> (x:lbls, stms)
                ) ([],[]) xs


-- | Función helper /seq/ que nos permite escribir
-- fácilmente una secuencia de [Stm] usando listas.
seqs :: [Stm] -> Stm
seqs []     = ExpS $ Const 0
seqs [s]    = s
seqs (x:xs) = Seq x (seqs xs)


procEntryExit2 :: Frame -> [Assem] -> [Assem]
procEntryExit2 frame stmts = [
        Oper {oassem = ".global " ++ (unpack $ name frame), osrc = [], odest = [], ojump=Nothing},
        Oper {oassem = ".type " ++ (unpack $ name frame) ++ ", @function", osrc = [], odest = [], ojump=Nothing},
        Lab {lassem = (unpack $ name frame)++ ":", label=name frame },
        Oper {oassem = "push `s0", osrc=[fp], odest=[sp],ojump=Nothing },
        Mov {massem = "mov `d0, `s0", mdest=fp, msrc=sp},
        Oper {oassem = "push `s0", osrc=[ebx], odest=[sp],ojump=Nothing },
        Oper {oassem = "push `s0", osrc=[esi], odest=[sp],ojump=Nothing },
        Oper {oassem = "push `s0", osrc=[edi], odest=[sp],ojump=Nothing },
        Oper {oassem = "sub `d0, " ++ show (frameSize frame), osrc = [], odest = [sp], ojump = Nothing}
    ] ++ stmts ++ [
        Oper {oassem = "add `d0, " ++ show (frameSize frame), osrc = [], odest = [sp], ojump = Nothing},
        Oper {oassem = "pop `d0", osrc = [], odest = [edi], ojump = Nothing},
        Oper {oassem = "pop `d0", osrc = [], odest = [esi], ojump = Nothing},
        Oper {oassem = "pop `d0", osrc = [], odest = [ebx], ojump = Nothing},
        Oper {oassem = "pop `d0", osrc = [], odest = [fp], ojump = Nothing},
        Oper {oassem = "ret", osrc = [eax], odest = [], ojump = Nothing}
    ]

instance Show Frag where
    show (Proc s f) = "Frame:" ++ show f ++ '\n': show s
    show (AString l ts) = show l ++ ":\n" ++ (foldr (\t ts -> ("\n\t" ++ unpack t) ++ ts) "" ts)
data Frame = Frame {
        name        :: Symbol,
        formals     :: [Bool],
        locals      :: [Bool],
        actualArg   :: Int,
        actualLocal :: Int,
        actualReg   :: Int
    }
    deriving Show

defaultFrame :: Frame
defaultFrame =  Frame {name = empty
                , formals = []
                , locals = []
                , actualArg = argsInicial
                , actualLocal = localsInicial
                , actualReg = regInicial}

-- TODOS A stack por i386
prepFormals :: Frame -> [Access]
prepFormals fs = reverse $  snd (foldl (\ (n,rs) _ -> (n+argsGap, InFrame n : rs) ) (argsInicial,[]) (formals fs))

newFrame :: Symbol -> [Bool] -> Frame
newFrame nm fs = defaultFrame {name = nm, formals = fs}

externalCall :: String -> [Exp] -> Exp
externalCall s = Call (Name $ pack s)

allocArg :: (Monad w, TLGenerator w) => Frame -> Bool -> w (Frame, Access)
allocArg fr _ =
    let actual = actualArg fr
        acc = InFrame $ wSz*actual + argsGap in
    return (fr{actualArg = actual +1}, acc)


allocLocal :: (Monad w, TLGenerator w) => Frame -> Bool -> w (Frame, Access)
allocLocal fr True =
    let actual = actualLocal fr
        acc = InFrame $ (-wSz)*actual + localsGap in
    return (fr{actualLocal= actual +1}, acc)
allocLocal fr False = do
    s <- newTemp
    return (fr, InReg s)

nextTemp :: Frame -> Int
nextTemp fr = localsGap + (-wSz)*(actualLocal fr + actualReg fr)

frameSize :: Frame -> Int
frameSize fr = wSz * (actualLocal fr + actualReg fr)

-- | Frame pointer
fp :: Temp
fp = pack "ebp"

-- | Stack pointer
sp :: Temp
sp = pack "esp"

eax :: Temp
eax = pack "eax"

ebx :: Temp
ebx = pack "ebx"

ecx :: Temp
ecx = pack "ecx"

edx :: Temp
edx = pack "edx"

esi :: Temp
esi = pack "esi"

edi :: Temp
edi = pack "edi"


registrosGenerales = [eax,ebx,ecx,edx,esi,edi]
k = Prelude.length registrosGenerales
todosLosRegistros = [eax,ebx,ecx,edx,esi,edi,fp,sp]
calldefs = [rv,ecx,edx]
specialregs = [rv, fp, sp]
calleesaves = [rv,ecx,edx]
callersaves = [ebx,fp,sp,esi,edi]
argregs = [] --Todos los parametros a stack.