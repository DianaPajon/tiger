import           TigerAbs
import           TigerEscap
import           TigerParser
import           TigerSymbol
import           Text.Parsec

import Tools

main :: IO ()
main =
  putStrLn "\n======= Test ESCAPES in progress =======" >>
  either (const redfail)  (const bluenice ) (calcularEEsc ejemplo1) >>
  either (const bluefail) (const rednice ) (calcularEEsc ejemplo2) >>
  test "./test/test_code" (const bluefail) (const rednice ) tester "escapa.tig" >>
  test "./test/test_code" (const bluefail) (const rednice ) tester "intro.tig" >>
  testDir good_loc (testSTDGood tester) >>
  putStrLn "Type:" >>
  testDir type_loc (testGood type_loc tester) >>
  putStrLn "\n======= Test FIN ======="

tester :: SourceName -> Either Errores Exp
tester s = either (fail $ "Testing Escapes: Parser error")
                calcularEEsc
         $ runParser expression () s s

ejemplo1 :: Exp -- La variable a escapa.
ejemplo1 = LetExp
            [ VarDec (pack "a") False Nothing (IntExp 1 (Simple 1 1)) (Simple 1 2)
            , FunctionDec
                    [ (pack "f1"
                      ,[( pack "a1", False , NameTy $ pack "int")]
                      ,Just $ pack "int"
                      ,VarExp (SimpleVar $ pack "a") (Simple 5 5)
                      , Simple 5 2)]
            ]
            (IntExp 42 (Simple 8 1))
            (Simple 1 0)

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
