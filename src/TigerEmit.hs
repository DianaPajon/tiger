module TigerEmit where
import TigerTree
import TigerTemp

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
    munchExp :: Exp -> w Temp
    munchStm :: Stm -> w ()

