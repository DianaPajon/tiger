module Main where

import TigerParser
import TigerTranslate
import TigerSeman
import Control.Monad.State
import TigerPrettyIr
import TigerFrame
import TigerTips
import TigerTrans
import TigerErrores
import TigerTree
import TigerCanon
import TigerPretty
import TigerPrettyIr
import TigerAbs
import TigerEmit
import Data.Text as T
import qualified TigerEscap as E
import System.IO.Unsafe
import Prelude as P
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

tester :: String -> Either [Errores] [[Assem]]
tester s =  tigerHastaAssem s

type TigerStage = Either [Errores]

{-
instance Monad TigerStage where
    return = Right
    (>>=) (Right a) f = f a
    (>>=) (Left err) f = Left err
-}


tigerHastaAssem :: String -> TigerStage [[Assem]]
tigerHastaAssem programa = do
    expresionSinEscapar <- parserStage programa
    expresionEscapada <- escapStage expresionSinEscapar
    (fragmentos,useed) <- translateStage expresionEscapada
    let (_,fragsSeparados) = sepFrag fragmentos
    let (assems,seed) = aplicarCodegen (fragsSeparados, useed)
    return $ P.map (\(frags, frame) -> frags) assems
  where aplanar (x:xs) = x ++ aplanar xs
        aplicarCodegen (frags, seed) =
            P.foldr 
                (\(stmts, frame) (programas,unique) -> 
                    let 
                        (codigoGenerado,nuevoSeed) = codegen stmts unique
                    in
                        ((codigoGenerado,frame):programas, nuevoSeed)
                )
                ([],seed)
                frags
                



parserStage :: String -> Either [Errores] TigerAbs.Exp
parserStage s = either (\err -> Left [Error $ T.pack $ "Error de parsing:" ++ show err] ) (\exp -> Right exp) (parse s)

escapStage :: TigerAbs.Exp -> Either [Errores] TigerAbs.Exp
escapStage exp = either (\err -> Left [err]) (\exp -> Right exp)  (E.calcularEEsc exp)

translateStage :: TigerAbs.Exp -> Either [Errores] ([Frag],Integer)
translateStage exp = either (\err -> Left err) (\(frags, estado) -> Right (frags, unique estado)) (runTranslate exp)


--Sandbox tools:


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


quickTest :: String -> String -> IO ()
quickTest dir file = test dir (badRes . show) (const $ bluenice) tester file

dirtyTestAssem :: String -> String -> Either [Errores] [[Assem]]
dirtyTestAssem dir file = tigerHastaAssem programa
   where programa = unsafePerformIO (readFile (dir ++ '/' : file))


printAssem :: [Assem] -> IO ()
printAssem (i@(Oper _ _ _ _):is) = do
     putStrLn $ oassem i ++ ", src:" ++ show (osrc i) ++ ", dest:" ++ show (mdest i)
     printAssem is
printAssem (i@(Mov _ _ _):is) = do
    putStrLn $ massem i ++ ", src:"  ++ show (msrc i) ++ ", dest:" ++ show (mdest i)
    printAssem is
printAssem (i@(Lab _ _):is) = do
    putStrLn $ lassem i
    printAssem is
printAssem [] = return ()
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


