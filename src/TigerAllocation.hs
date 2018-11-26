module TigerAllocation where
import TigerAssem
import Grafo
import TigerLiveness
import TigerShow 
import TigerFrame
import TigerTemp
import Data.Set
data AllocState = 
 AS {
     assembly :: [Assem]
    ,stack :: [(StackElement, Grafo Temp, Set (Temp, Temp))] --Stack del grafo de liveness.
    ,interf :: Grafo Temp
    ,moves :: Set (Temp, Temp)
  }

data StackElement = Select Temp | Spill Temp

class (Monad a, TLGenerator a) => Allocator a where
    push :: StackElement -> a ()

retrieveTemp t fr = Oper {oassem="movl `d0, [ebp +" ++ show (nextTemp fr) ++ "]",osrc=[],odest=[t],ojump = Nothing }
saveTemp t fr = Oper {oassem="movl [ebp +" ++ show (nextTemp fr) ++ "],`s0",osrc=[t],odest=[],ojump = Nothing }

spillNode :: (Allocator a) => [Assem] -> Frame -> Temp -> a ([Assem],Frame)
spillNode [] fr t = return ([],fr{actualReg = actualReg fr +1})
spillNode (op@(Oper _ dest src  _):ops) fr t = do
    let define = elem t dest
    let usa = elem t src
    te <- newTemp
    (ops',nuevoFrame) <- spillNode ops fr t
    if(usa || define)
        then let previa = if usa then [retrieveTemp te fr] else []
                 siguiente = if define then [saveTemp te fr] else []
             in return (previa ++ [op] ++ siguiente ++ ops', nuevoFrame)
        else return (op:ops',nuevoFrame)
spillNode (op@(Mov _ dest src  ):ops) fr t = do
    let define =  t ==  dest
    let usa = t == src
    te <- newTemp
    (ops',nuevoFrame) <- spillNode ops fr t
    if(usa || define)
        then let previa = if usa then [retrieveTemp te fr] else []
                 siguiente = if define then [saveTemp te fr] else []
             in return (previa ++ [op] ++ siguiente ++ ops', nuevoFrame)
        else return (op:ops',nuevoFrame)
spillNode (op:ops) fr t = do
    (ops',nuevoFrame) <- spillNode ops fr t
    return (op:ops',nuevoFrame)