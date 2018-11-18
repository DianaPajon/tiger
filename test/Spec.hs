import TigerParser
import TigerTranslate
import TigerSeman
import Control.Monad.State
import TigerPrettyIr
import TigerFrame
import TigerTips
import TigerTrans
import TigerErrores
--For debuggin's sake
import TigerPretty
import TigerPrettyIr
import TigerAbs

import           Data.Text as T
import qualified TigerEscap as E
import Tools

import System.IO.Unsafe

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

tester :: String -> Either [Errores] ([Frag],Estado)
tester s =   either (\err -> Left [Error (T.pack $ show err)]) (\exp -> runTranslate exp) (
                  either (\err -> Left $ Interno (T.pack $ "Error de parsing" ++ show err)) (\exp -> E.calcularEEsc exp) (parse s)
            )

parserStage :: String -> Either [Errores] TigerAbs.Exp
parserStage s = either (\err -> Left [Error $ T.pack $ "Error de parsing:" ++ show err] ) (\exp -> Right exp) (parse s)

escapStage :: TigerAbs.Exp -> Either [Errores] TigerAbs.Exp
escapStage exp = either (\err -> Left [err]) (\exp -> Right exp)  (E.calcularEEsc exp)

translateStage :: TigerAbs.Exp -> Either [Errores] [Frag]
translateStage exp = either (\err -> Left err) (\(frags, estado) -> Right frags) (runTranslate exp)

unicaPosicion :: Pos
unicaPosicion = Simple {line = 0, col = 0}

programaPrueba :: TigerAbs.Exp
programaPrueba = 
    LetExp 
      [
        TypeDec [(T.pack "enteros", NameTy $ T.pack "int", unicaPosicion)],
        VarDec (T.pack "variableEntera") Escapa (Just $ T.pack "enteros") (IntExp 2 unicaPosicion) unicaPosicion,
        VarDec (T.pack "variableEntera2") Escapa (Just $ T.pack "enteros") (IntExp 4 unicaPosicion) unicaPosicion
      ]
      (StringExp "hola" unicaPosicion)
      unicaPosicion
  
runTranslate :: TigerAbs.Exp -> Either [Errores] ([Frag],Estado)
runTranslate expre = runStateT (transProg expre :: TigerState [Frag])  initConf


--END CODIGO PRUEBA
--Sandbox tools:
quickTest :: String -> String -> IO ()
quickTest dir file = test dir (badRes . show) (const $ bluenice) tester file

dirtyTest :: String -> String -> Either String Estado
dirtyTest dir file = 
  either 
  (\err -> Left $ show err) 
  (\exp -> either (\err -> Left $ show err) (
      \(exp,(env,lista)) -> either (\err -> Left $  show err) (\(frags, estado) -> Right estado) $ runTranslate exp
  ) $ E.calcularEscStepper exp) 
  (parse programa)
   where programa = unsafePerformIO (readFile (dir ++ '/' : file))

getProgram dir file = unsafePerformIO (readFile (dir ++ '/' : file))

right (Right a) = a
left (Left a) = a

frame (Proc stmt frame) = frame

prettyAst dir file =
    either 
    (\err -> Left $ show err) 
    (\exp -> either (\err -> Left $ show err) (\(exp,(env,lista)) -> Right $ renderExp exp) $ E.calcularEscStepper exp) 
    (parse programa)
     where programa = unsafePerformIO (readFile (dir ++ '/' : file))

prettyIr dir file fragNumber = putStrLn $ renderFrag $ (frags $ right $ dirtyTest dir file) !! fragNumber

prettyFrame dir file fragNumber = frame $ (frags $ right $ dirtyTest dir file) !! fragNumber