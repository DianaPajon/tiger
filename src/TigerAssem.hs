module TigerAssem where

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
  } deriving (Eq, Show)