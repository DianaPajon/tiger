module TigerFrame where

import           TigerTemp
import           TigerTree

import           TigerSymbol

import           Prelude     hiding (exp)

--

-- | Frame pointer
fp :: Temp
fp = pack "FP"

-- | Stack pointer
sp :: Temp
sp = pack "SP"

-- | Return value
rv :: Temp
rv = pack "RV"

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
fpPrevLev = 0

argsGap = wSz
localsGap = 4

argsInicial = 0
regInicial = 1
localsInicial = 0

calldefs = [rv]
specialregs = [rv, fp, sp]

data Access = InFrame Int | InReg Temp
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
allocArg fr True =
    let actual = actualArg fr
        acc = InFrame $ actual + argsGap in
    return (fr{actualArg = actual +1}, acc)
allocArg fr False = do
    s <- newTemp
    return (fr, InReg s)

allocLocal :: (Monad w, TLGenerator w) => Frame -> Bool -> w (Frame, Access)
allocLocal fr True =
    let actual = actualLocal fr
        acc = InFrame $ actual + localsGap in
    return (fr{actualLocal= actual +1}, acc)
allocLocal fr False = do
    s <- newTemp
    return (fr, InReg s)

auxexp :: Int -> Exp
auxexp 0 = Temp fp
auxexp n = Mem(Binop Plus (auxexp (n-1)) (Const fpPrevLev))

exp :: Access -> Int -> Exp
exp (InFrame k) e = Mem(Binop Plus (auxexp e) (Const k))
exp (InReg l) _   = Temp l
