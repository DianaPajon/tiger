import TigerParser
import TigerState
import TigerTips
import TigerTrans
--For debuggin's sake
import TigerPretty
import TigerPrettyIr

import           Data.Text as T
import qualified TigerEscap as E
import Tools
main :: IO ()
main = 
    putStrLn "\n======= Test Spec in progress =======" >>
    putStrLn "escapa.tig" >>
    ( test "./test/test_code" (badRes . show) (const $ bluenice) tester "escapa.tig") >>
    putStrLn "intro.tig" >>
    ( test "./test/test_code" (badRes . show) (const $ bluenice) tester "intro.tig") >>
    putStrLn "Good:" >>
    testDir good_loc (testGood good_loc tester) >>
    putStrLn "Type:" >>
    testDir type_loc (testGood type_loc tester) >>
    putStrLn "Bad:" >>
    testDir bad_loc (testBad bad_loc tester) >>
    putStrLn "\n======= Test FIN ======="

tester :: String -> Either [Errores] ((BExp, Tipo),Estado)
tester s =   either (\err -> Left []) (\exp -> runTranslate exp) (
                  either (\err -> Left $ E.Interno (T.pack "Error de parsing")) (\exp -> E.calcularEEsc exp) (parse s)
            )
quickTest :: String -> String -> IO ()
quickTest dir file = test dir (badRes . show) (const $ bluenice) tester file