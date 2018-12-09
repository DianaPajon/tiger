{-# LANGUAGE BangPatterns         #-}

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
import TigerShow
import TigerLiveness
import TigerTemp
import Grafo
import Data.Text as T
import Data.Set as S
import qualified TigerEscap as E
import System.IO.Unsafe
import Prelude as P
import Tools
import TigerAllocation

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

tester :: String -> Either [Errores] [([Assem], Frame)]
tester s = (\ !a -> a ) (tigerHastaAllocation s)

type TigerStage = Either [Errores]

{-
version "final" de tiger, dado un programa, imprime assembly
-}

tiger :: String -> TigerStage String
tiger programa = do
    expresionSinEscapar <- parserStage programa
    expresionEscapada <- escapStage expresionSinEscapar
    (fragmentos,useed) <- translateStage expresionEscapada
    let (labels,intermedio) = sepFrag fragmentos
    let (assembly,seed) = aplicarCodegen (intermedio, useed)
    let assemblyAlocado = P.map (\(assems,frame) -> allocate assems frame seed) assembly --El seed ya no importa
    let procedimientos = P.map (\(assems,frame) -> procEntryExit2 frame assems) assemblyAlocado --El frame ya no importa
    return $ makeProgram procedimientos labels
  where aplanar (x:xs) = x ++ aplanar xs
        aplicarCodegen (frags, seed) =
            P.foldr 
                (\(stmts, frame) (programas,unique) -> 
                    let 
                        (codigoGenerado,nuevoSeed) = codegen stmts unique
                    in
                        ( (codigoGenerado ,frame):programas, nuevoSeed)
                )
                ([],seed)
                frags


tigerHastaAllocation :: String -> TigerStage [([Assem], Frame)]
tigerHastaAllocation programa = do
    (frames,seed) <- tigerHastaAssem programa
    return $ P.map (\(assems,frame) -> allocate assems frame seed) frames

tigerHastaLiveness :: String -> TigerStage [Grafo Temp]
tigerHastaLiveness programa = do
    (frames,seed) <- tigerHastaAssem programa
    return $  P.map (\(instrucciones, frame) -> liveness instrucciones) frames

tigerHastaAssem :: String -> TigerStage ([([Assem], Frame)],Integer)
tigerHastaAssem programa = do
    expresionSinEscapar <- parserStage programa
    expresionEscapada <- escapStage expresionSinEscapar
    (fragmentos,useed) <- translateStage expresionEscapada
    let (_,fragsSeparados) = sepFrag fragmentos
    let (assems,seed) = aplicarCodegen (fragsSeparados, useed)
    return (assems,seed)
  where aplanar (x:xs) = x ++ aplanar xs
        aplicarCodegen (frags, seed) =
            P.foldr 
                (\(stmts, frame) (programas,unique) -> 
                    let 
                        (codigoGenerado,nuevoSeed) = codegen stmts unique
                    in
                        ( (codigoGenerado ,frame):programas, nuevoSeed)
                )
                ([],seed)
                frags

tigerHastaTranslate :: String -> TigerStage ([Frag], Integer)
tigerHastaTranslate programa = do
    expresionSinEscapar <- parserStage programa
    expresionEscapada <- escapStage expresionSinEscapar
    translateStage expresionEscapada



parserStage :: String -> Either [Errores] TigerAbs.Exp
parserStage s = either (\err -> Left [Error $ T.pack $ "Error de parsing:" ++ show err] ) (\exp -> Right exp) (parse s)

escapStage :: TigerAbs.Exp -> Either [Errores] TigerAbs.Exp
escapStage exp = either (\err -> Left [err]) (\exp -> Right exp)  (E.calcularEEsc exp)

translateStage :: TigerAbs.Exp -> Either [Errores] ([Frag],Integer)
translateStage exp = either (\err -> Left err) (\(frags, estado) -> Right (frags, TigerTranslate.unique estado)) (runTranslate exp)


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
  
runTranslate :: TigerAbs.Exp -> TigerStage ([Frag],Estado)
runTranslate expre = runStateT (transProg expre :: TigerState [Frag])  initConf


quickTest :: String -> String -> IO ()
quickTest dir file = test dir (badRes . show) (const $ bluenice) tester file

dirtyTestAssem :: String -> String -> TigerStage [([Assem], Frame)]
dirtyTestAssem dir file = do
    (ret,unique) <- tigerHastaAssem programa
    return ret
   where programa = unsafePerformIO (readFile (dir ++ '/' : file))


tigerHastaFlow :: String -> TigerStage [(FlowGraph Assem, [FlowInfo Assem])]
tigerHastaFlow programa = do
    (frames,seed) <- tigerHastaAssem programa
    return $ P.map (\(instrucciones, frame) -> fullFlow instrucciones) frames
   
printInfo :: FlowGraph Assem -> FlowInfo Assem -> String
printInfo grafo info = 
    let
        nodoGrafo = nodo info
        sucesores = Grafo.succ grafo nodoGrafo
        instruccion = dato nodoGrafo
        assembly = case instruccion of
            (Oper assemb _ _ _) -> assemb
            (Mov assemb _ _ ) -> assemb
            (Lab assem _ ) -> assem
        liveIns  = "LiveIn: " ++ P.foldl (\temps temp -> temps ++ ", " ++ unpack temp) "" (liveIn info)
        liveOuts = "LiveOut: " ++ P.foldl (\temps temp -> temps ++ ", " ++ unpack temp) "" (liveOut info)
        numero = show $ ord nodoGrafo
        numerosSucesores = "Succ:" ++ (P.foldl (\succs suc -> succs ++ ", " ++ show (ord suc)) "" $ toList sucesores)
    in numero ++ ":" ++ assembly ++ "   ||   " ++ liveIns ++ " " ++ liveOuts ++ " " ++ numerosSucesores

printInfos :: FlowGraph Assem -> [FlowInfo Assem] -> String
printInfos grafo infos = P.foldl (\infos info -> infos ++ "\n" ++ printInfo grafo info) "" infos

dirtyTestFlow :: String -> String -> TigerStage [(FlowGraph Assem, [FlowInfo Assem])]
dirtyTestFlow dir file = tigerHastaFlow programa
  where programa = unsafePerformIO (readFile (dir ++ '/' : file))

--dirtyTestLiveness :: String -> String -> TigerStage [[LivenessInfo ()]]
--dirtyTestLiveness dir file = tigerHastaLiveness programa
   --where programa = unsafePerformIO (readFile (dir ++ '/' : file))

dirtyTestAllocation :: String -> String -> TigerStage [([Assem], Frame)]
dirtyTestAllocation dir file = tigerHastaAllocation programa
  where programa = unsafePerformIO (readFile (dir ++ '/' : file))

dirtyTiger :: String -> String -> TigerStage String
dirtyTiger dir file = tiger programa
  where programa = unsafePerformIO (readFile (dir ++ '/' : file))

imprimirFrags :: String -> String -> IO ()
imprimirFrags dir file = do
    let frags = fst $ right $ tigerHastaTranslate $ unsafePerformIO (readFile (dir ++ '/' : file))
    let strings = P.map (renderFrag) frags
    nadas <- mapM putStrLn strings
    return ()

imprimirAssembler :: String -> String -> IO ()
imprimirAssembler dir file = do
    let bloques = right $ dirtyTestAssem dir file
    nada <- mapM (\(ins, fr) -> mostrarBloque ins fr) bloques
    return ()

imprimirAllocation :: String -> String -> IO ()
imprimirAllocation dir file = do
    let bloques = right $ dirtyTestAllocation dir file
    nada <- mapM (\(ins, fr) -> mostrarBloque ins fr) bloques
    return ()

imprimirAssembly :: String -> String -> IO ()
imprimirAssembly dir file = do
    let codigo = right $ dirtyTiger dir file
    putStrLn codigo

imprimirFlow :: String -> String -> IO ()
imprimirFlow dir file = do
    let flows = right $ dirtyTestFlow dir file
    let flowDatas = P.map (\(grafo, infos) -> printInfos grafo infos) flows
    nadas <- mapM putStrLn flowDatas
    return ()


mostrarBloque :: [Assem] -> Frame -> IO ()
mostrarBloque inss frame = putStrLn $ printCode inss

showAssem :: [Assem] -> String
showAssem (i@(Oper assembly dest src _):is) = 
   assembly ++ ", src:" ++ show src ++ ", dest:" ++ show dest ++ "\n" ++
   showAssem is
showAssem (i@(Mov assembly dest src):is) = 
   assembly  ++ ", src:"  ++ show src ++ ", dest:" ++ show dest ++ "\n" ++ 
   showAssem is
showAssem (i@(Lab assembly _):is) = 
   assembly ++ "\n" ++
   showAssem is
showAssem [] = ""


printAssem :: [Assem] -> IO ()
printAssem assems = putStrLn $ showAssem assems

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


