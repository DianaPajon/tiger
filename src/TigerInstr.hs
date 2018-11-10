module TigerInstr where
import TigerTree
import TigerTemp

data Assem =
    Oper {
        oassem :: String, 
        odest :: [Temp], 
        osrc :: [Temp], 
        ojump :: Maybe [Label]
    }
  | Label {
        lassem :: String,
        label :: Label
   }
  | Move {
      massem :: String,
      mdest :: Temp,
      msrc :: Temp
  }

class TLGenerator w => Emisor w where
    emit :: Assem -> w ()
    getCode :: w [Assem]
    munchExp :: Tree.Exp -> w Temp
    munchStm :: Tree.Stm -> w ()

