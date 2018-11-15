module TigerEmit where
import Prelude as P
import TigerTree
import TigerTemp
import Data.Text
import TigerFrame

data Assem =
    Oper {
        oassem :: String, 
        odest :: [Temp], 
        osrc :: [Temp], 
        ojump :: Maybe [Label]
    }
  | Lab {
        lassem :: String,
        label :: Label
   }
  | Mov {
        massem :: String,
        mdest :: Temp,
        msrc :: Temp
  }

class TLGenerator w => Emisor w where
    emit :: Assem -> w ()
    getCode :: w [Assem]



munchArgs [] = return ()
munchArgs (es) = do
    t <- munchExp $ P.last es
    emit Oper {
        oassem = "pushl `s0"
       ,osrc = [t]
       ,odest = [sp]
       ,ojump = Nothing
    }
    munchArgs $ P.init es
--Maximum munch para ia32. 
munchExp (Call (Name n) par) = do
    munchArgs par
    emit Oper {
        oassem = "call " ++ unpack n
       ,osrc = []
       ,odest = eax:calldefs 
       ,ojump = Just [n]
    }
    --al volver, lo primero es normalizar los argumentos. Ya call-ret eliminó el return value.
    emit Oper {
        oassem = "addl $" ++ (show (P.length par * wSz)) ++ ", `d0"
       ,osrc = []
       ,odest = [sp]
       ,ojump = Nothing
    }
    return eax --Return value
--Addressing con offset, para ahorrar una operacion
munchExp (Mem (Binop Plus e1 (Const i))) = do
    src <- munchExp e1
    dest <- newTemp
    emit Oper {
        oassem = "movl " ++ show i ++ "(`s0)" ++ ", `d0"
       ,osrc = [src]
       ,odest = [dest]
       ,ojump = Nothing
    }
    return dest
munchExp (Mem (Binop Plus (Const i) e1)) = do
    src <- munchExp e1
    dest <- newTemp
    emit Oper {
        oassem = "movl " ++ show i ++ "(`s0)" ++ ", `d0"
       ,osrc = [src]
       ,odest = [dest]
       ,ojump = Nothing
    }
    return dest
munchExp (Mem (Binop Minus e1 (Const i))) = do
    src <- munchExp e1
    dest <- newTemp
    emit Oper {
        oassem = "movl " ++ show (-i) ++ "(`s0)" ++ ", `d0"
       ,osrc = [src]
       ,odest = [dest]
       ,ojump = Nothing
    }
    return dest
munchExp (Mem (Binop Minus (Const i) e1)) = do
    src <- munchExp e1
    dest <- newTemp
    emit Oper {
        oassem = "movl " ++ show (-i) ++ "(`s0)" ++ ", `d0"
       ,osrc = [src]
       ,odest = [dest]
       ,ojump = Nothing
    }
    return dest
--Ver si se puede sumar a un label.
munchExp (Mem (Name l)) = do
    let label = unpack l
    dest <- newTemp
    emit Oper {
        oassem = "movl (" ++ label ++ "), `d0"
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
            emit Oper {
                oassem = "addl `s0,`s1"
               ,osrc = [t1,t2]
               ,odest = [t1]
               ,ojump = Nothing
            }
            return t1
        Minus ->  do
            emit Oper {
                oassem = "subl`s0,`s1"
               ,osrc = [t1,t2]
               ,odest = [t1]
               ,ojump = Nothing
            }
            return t1
        Mul -> do 
            emit Oper {
                oassem = "movl %eax,t1"
               ,osrc = [t1]
               ,odest = [eax]
               ,ojump = Nothing
            }
            emit Oper {
                oassem = "mul `s0"
               ,osrc = [t2]
               ,odest = [eax,edx]
               ,ojump = Nothing
            }
            return eax
        Div -> do 
            emit Oper {
                oassem = "movl %eax,t1"
               ,osrc = [t1]
               ,odest = [eax]
               ,ojump = Nothing
            }
            emit Oper {
                oassem = "div `s0"
               ,osrc = [t2]
               ,odest = [eax,edx]
               ,ojump = Nothing
            }
            return eax
        And -> do
            emit Oper {
                oassem = "and `s0,`s1"
               ,osrc = [t1,t2]
               ,odest = [t1]
               ,ojump = Nothing
            }
            return t1
        Or -> do
            emit Oper {
                oassem = "or `s0,`s1"
               ,osrc = [t1,t2]
               ,odest = [t1]
               ,ojump = Nothing
            }
            return t1
-- ¿Se va a dar este caso alguna vez?. Pareciera imposible por donde se lo mire... está en el libro.
munchExp (Mem (Const i)) = do
    dest <- newTemp
    emit Oper {
        oassem = "movl (" ++ show i ++ "), `d0"
        ,odest = [dest]
        ,osrc = []
        ,ojump = Nothing
    }
    return dest
munchExp (Temp t) = return t
mucnhExp (Eseq s e) = do
    munchStm s
    mucnhExp e



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
                                TigerTree.EQ -> "je"
                                TigerTree.NE -> "jne"
                                TigerTree.LT -> "jl"
                                TigerTree.GT -> "jg"
                                TigerTree.LE -> "jle"
                                TigerTree.GE -> "jge"
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

munchFrag (Proc stmt frame) = munchProc stmt Frame
munchFrag (AString l s) = munchString l s

munchProc stmt frame = undefined
munchString = undefined
