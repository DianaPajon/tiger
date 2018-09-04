import           TigerAbs
import           TigerEscap
import           TigerParser (parse)
import           TigerQQ
import           TigerSymbol

import           Tools

main :: IO ()
main =
  putStrLn "\n======= Test ESCAPES in progress =======" >>
  either (const rednice)  (const bluefail) (calcularEEsc ejemplo1) >>
  either (const redfail) (const bluenice) (calcularEEsc ejemplo2) >>
  putStrLn "\n======= [escapa.tig, intro.tig] =======" >>
  test "./test/test_code" (const bluefail) (const rednice ) tester "escapa.tig" >>
  test "./test/test_code" (const redfail) (const bluenice) tester "intro.tig" >>
  putStrLn "\n======= Good loc =======" >>
  testDir good_loc (testSTDGood tester) >>
  putStrLn "Type:" >>
  testDir type_loc (testGood type_loc tester) >>
  putStrLn "\n======= Test FIN ======="

tester :: String -> Either Errores Exp
tester = either (fail $ "Testing Escapes: Parser error")
                calcularEEsc
         . parse

ejemplo1 :: Exp -- La variable a escapa.
ejemplo1 = [expr|
                let
                  var a : int := 1
                  function f1(a : int):= a
                in
                  f1(a)
                end|]

ejemplo2 :: Exp -- La variable b no está definida.
ejemplo2 = LetExp
            [ VarDec (pack "a") False Nothing (IntExp 1 (Simple 1 2)) (Simple 1 2)
            -- , VarDec "b" Nothing Nothing (IntExp 2 1) 2
            -- , VarDec "c" Nothing Nothing (IntExp 3 1) 3
            , FunctionDec
                    [ (pack "f1"
                      ,[(pack "a1", False , NameTy $ pack "int")]
                      , Just $ pack "int",VarExp (SimpleVar $ pack "b") (Simple 5 5)
                      ,(Simple 5 6))
                    ]
            ]
            (IntExp 42 (Simple 8 1))
            (Simple 1 0)
