{-# Language OverloadedStrings #-}
import Tools
import TigerTree
import TigerFrame
import TigerInterp
import TigerSymbol

import Data.Map as M

main :: IO ()
main =
  let cpu = CPU (M.fromList [ (rv , 0)
                          , (fp , 0)
                          , (sp , 0)
                          , (pack "r1", 15)
                          , (pack "r2", 100)
                          ])
                (M.fromList [(1, Str "hola")])
                (M.fromList [ ("str1" , GG 1)])
                []
                []
  in
  putStrLn "\n======= Test Interpreter n progress =======" >>
  -- if (testinterp cpu [(res sumareg)] 115) then
  --   bluenice
  -- else
  --   putStrLn "FAIL" >>
  print (testinterp cpu [(res callP)] 1) >>
  putStrLn "\n======= Test FIN ======="

testinterp :: CPU -> [Stm] -> Int -> Bool
testinterp cpu stm = ((mem (runInitial cpu stm) ! rv) ==)

suma :: Exp
suma = Binop Plus (Const 14) (Const 15)

sumareg :: Exp
sumareg = Binop Plus (Temp (pack "r1")) (Temp (pack "r2"))

res :: Exp -> Stm
res = Move (Temp rv)

callP = Call (Name "print") [Name "str1"]
-- print("hola")
