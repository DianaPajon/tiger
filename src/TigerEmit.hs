module TigerEmit (
        codegen,  Assem(Oper, Mov, Lab), oassem, odest, osrc, ojump, lassem, label, massem, mdest, msrc
    ) where

import Prelude as P
import TigerTree
import TigerTemp
import Data.Text
import TigerFrame
import TigerCanon
import TigerAssem
import Data.Map as M
import Control.Monad.State



class (Monad w, TLGenerator w) => Emisor w where
    emit :: Assem -> w ()


munchArgs :: (Emisor e) => [Exp] -> e ()
munchArgs [] = return ()
munchArgs (es) = do
    t <- munchExp $ P.last es
    emit Oper {
        oassem = "push `s0"
       ,osrc = [t]
       ,odest = [sp]
       ,ojump = Nothing
    }
    munchArgs $ P.init es

munchExp :: (Emisor w) =>  Exp -> w Temp
munchExp (Call (Name n) par) = do
    munchArgs par
    emit Oper {
        oassem = "call " ++ unpack n
       ,osrc = []
       ,odest = calldefs 
       ,ojump = Nothing -- Considero un call DISTINTO a un jump, ya que para liveness debería ser así.
    }
    --al volver, lo primero es normalizar los argumentos. Ya call-ret eliminó el return value.
    emit Oper {
        oassem = "add `d0, "  ++ (show (P.length par * wSz))
       ,osrc = []
       ,odest = [sp]
       ,ojump = Nothing
    }
    return rv --Return value
--Addressing con offset, para ahorrar una operacion
munchExp (Mem (Binop Plus e1 (Const i))) = do
    src <- munchExp e1
    dest <- newTemp
    emit Oper {
        oassem = "mov `d0, [`s0 " ++ signedShow i ++ "]"
       ,osrc = [src]
       ,odest = [dest]
       ,ojump = Nothing
    }
    return dest
munchExp (Mem (Binop Plus (Const i) e1)) = do
    src <- munchExp e1
    dest <- newTemp
    emit Mov {
        massem = "mov `d0, [`s0 + " ++ show i ++ "]"
       ,msrc = src
       ,mdest = dest
    }
    return dest
munchExp (Mem (Binop Minus e1 (Const i))) = do
    src <- munchExp e1
    dest <- newTemp
    emit Mov {
        massem = "mov `d0, [`s0 - " ++ show i ++ "]"
       ,msrc = src
       ,mdest = dest
    }
    return dest
munchExp (Mem (Binop Minus (Const i) e1)) = do
    src <- munchExp e1
    dest <- newTemp
    emit Mov {
        massem = "mov `d0, [`s0  - " ++ show i ++ "]"
       ,msrc = src
       ,mdest = dest
    }
    return dest
--Ver si se puede sumar a un label.
munchExp (Mem (Name l)) = do
    let label = unpack l
    dest <- newTemp
    emit Oper {
        oassem = "mov `d0, " ++ label 
       ,odest = [dest]
       ,osrc = []
       ,ojump = Nothing
    }
    return dest
munchExp (Binop oper e1 e2) = do
    t1 <- munchExp e1
    t2 <- munchExp e2
    case oper of
        Plus -> do
            dest <- newTemp
            emit Mov {
                massem = "mov `d0,`s0",
                msrc  = t2,
                mdest = dest
            }
            emit Oper {
                oassem = "add `s1,`s0"
               ,osrc = [t1,dest]
               ,odest = [dest]
               ,ojump = Nothing
            }
            return dest
        Minus ->  do
            dest <- newTemp
            emit Mov {
                massem = "mov `d0,`s0",
                msrc  = t1,
                mdest = dest
            }
            emit Oper {
                oassem = "sub `s1,`s0"
               ,osrc = [t2,dest]
               ,odest = [dest]
               ,ojump = Nothing
            }
            return dest
        Mul -> case unpack t2 of 
                    "eax" -> do
                                emit Oper {
                                    oassem = "mul `s0"
                                ,osrc = [t1]
                                ,odest = [eax,edx]
                                ,ojump = Nothing
                                }
                                return eax
                    _ -> do
                            emit Mov {
                                massem = "mov `d0, `s0"
                            ,msrc = t1
                            ,mdest = eax
                            }
                            emit Oper {
                                oassem = "mul `s0"
                            ,osrc = [t2]
                            ,odest = [eax,edx]
                            ,ojump = Nothing
                            }
                            return eax
        Div -> case unpack t2 of
                    "eax" -> do emit Oper {
                                    oassem = "div `s0"
                                    ,osrc = [t1]
                                    ,odest = [eax,edx]
                                    ,ojump = Nothing
                                }
                                return eax
                    _ -> do
                            emit Mov {
                                massem = "mov `d0,`s0"
                            ,msrc = t1
                            ,mdest = eax
                            }
                            emit Oper {
                                oassem = "div `s0"
                            ,osrc = [t2]
                            ,odest = [eax,edx]
                            ,ojump = Nothing
                            }
                            return eax
        And -> do
            dest <- newTemp
            emit Mov {
                massem = "mov `d0,`s0",
                msrc  = t2,
                mdest = dest
            }
            emit Oper {
                oassem = "and `s1,`s0"
               ,osrc = [t1,dest]
               ,odest = [dest]
               ,ojump = Nothing
            }
            return dest
        Or -> do
            dest <- newTemp
            emit Mov {
                massem = "mov `d0,`s0",
                msrc  = t2,
                mdest = dest
            }
            emit Oper {
                oassem = "or `s1,`s0"
               ,osrc = [t1,dest]
               ,odest = [dest]
               ,ojump = Nothing
            }
            return dest
-- ¿Se va a dar este caso alguna vez?. Pareciera imposible por donde se lo mire... está en el libro.
munchExp (Mem (Const i)) = do
    dest <- newTemp
    emit Oper {
        oassem = "mov `d0, [" ++ show i ++ "]"
       ,odest = [dest]
       ,osrc = []
       ,ojump = Nothing
    }
    return dest
munchExp (Mem e) = do
    t <- munchExp e
    dest <- newTemp
    emit Oper {
        oassem = "mov `d0, [`s0]"
       ,odest = [dest]
       ,osrc = [t]
       ,ojump = Nothing
    }
    return dest
munchExp (Temp t) = return t
munchExp (Eseq s e) = do
    munchStm s
    t <- munchExp e
    return t
munchExp (Const n) = do
    dest <- newTemp
    emit Oper {
        oassem = "mov `d0, " ++  show n 
       ,odest = [dest]
       ,osrc = []
       ,ojump = Nothing
    }
    return dest
munchExp (Name l) = do
    dest <- newTemp
    emit Oper {
        oassem = "mov `d0, offset "  ++ unpack l 
       ,odest = [dest]
       ,osrc = []
       ,ojump = Nothing
    }
    return dest
munchExp def = error $ show def

--Caso particular, Mem usado en el lado izquierdo de un move.--FODO: Registrer addessing con offset.
munchStm :: (Emisor e) =>  Stm -> e ()
munchStm (Move (Mem e1) e2) = do
    t1 <- munchExp e1
    t2 <- munchExp e2
    emit Oper {
        oassem = "mov [`s0], `s1"
        ,osrc = [t1,t2]
        ,odest = []
        ,ojump = Nothing
    }
--Otro caso particular, un label el el lado izquierdo de un move.
munchStm (Move (Name l1) e2) = do
    t2 <- munchExp e2
    let label = unpack l1
    emit Oper {
        oassem = "mov [label], `s0"
        ,osrc = [t2]
        ,odest = []
        ,ojump = Nothing
    }
--Caso general e1 puede ser un registro solamente, es el caso "Mover a un registro". Obvia optimizacion usar un solo registro.
munchStm (Move e1 e2) = do
    t2 <- munchExp e2
    t1 <- munchExp e1
    emit Mov {
        massem = "mov `d0, `s0"
       ,msrc = t2
       ,mdest = t1
    }
munchStm (Jump (Name l1) l2) = do
    let label = unpack l2
    emit Oper {
        oassem = "jmp " ++ label
        ,osrc = []
        ,odest = []
        ,ojump = Just [l2]
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
    --En código canonizado... esto podría ser saltar a la siguiente instrucción- 
    --Por ahora lo dejo
    emit Oper {
        oassem = "jmp " ++ falseDest
        ,osrc = []
        ,odest = []
        ,ojump = Just [fl]
    }
--Instrucciones de salto condicional de x86 según el operador.
    where jumpInstruction op = case op of
                                TigerTree.EQ -> "je"
                                TigerTree.NE -> "jne"
                                TigerTree.LT -> "jl"
                                TigerTree.GT -> "jg"
                                TigerTree.LE -> "jle"
                                TigerTree.GE -> "jge"
--Casos simples
munchStm (Label l) = do
    let ls = unpack l
    let ins = ls ++ ":"
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

munchStmts :: (Emisor e) =>  [Stm] -> e ()
munchStmts [] = return ()
munchStmts (s:ss) = do
    munchStm s 
    munchStmts ss 
  


--Implementación del emisor de código

data EstadoEmisor = EstadoEmisor {
    assembly :: [Assem],
    unique :: Integer,
    tank:: M.Map Label [Stm]
} deriving Show

type Emit = State EstadoEmisor

instance TLGenerator Emit where
    newTemp = do estado <- get
                 let u = unique estado
                 put estado{unique = u + 1}
                 return $ pack ("t" ++ show u)
    newLabel = do estado <- get
                  let u = unique estado
                  put estado{unique = u + 1}
                  return $ pack ("L" ++ show u)

instance Emisor Emit where 
    emit ins = do
        e <- get
        put  e{assembly = assembly e ++ [ins]}

instance Trackable Emit where
    enterBlock' l b = do
        st <- get
        put $ st{tank = insert l b $ tank st}
    getBlock l = do
        s <- get
        return $ M.lookup l (tank s)

munchProc :: (Emisor w, Trackable w) => Stm -> w ()
munchProc s  = do
    lin <- linearize s
    lss <- basicBlocks lin
    stmts <- traceSchedule lss
    munchStmts stmts
    emit Oper {
        oassem= "",
        odest=[],
        osrc=calleesaves++[sp,fp],
        ojump=Nothing
    }

codegen :: Stm ->  Integer -> ([Assem], Integer)
codegen cuerpo seed = (assembly estado,  unique estado)
 where estado = snd $ runState (munchProc cuerpo ) (EstadoEmisor{unique = seed, assembly = [], tank = M.empty})


signedShow :: (Integral a, Show a ) => a -> String
signedShow x = if( x >= 0) then "+" ++ show x else show x