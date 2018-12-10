module Main (main) where
import           Control.Monad
import           Control.Monad.State   hiding (evalState)
import           Data.Either
import           Data.Maybe
import Data.Text as T
import           System.Console.GetOpt
import qualified System.Environment    as Env
import           System.Exit
import           System.IO
import           TigerAbs
import           TigerEscap as E
import           TigerParser
import           TigerPretty
import           TigerSeman
import           TigerTemp
import           TigerUnique
import           TigerTranslate
import           TigerEmit
import           Text.Parsec           (runParser)
import           TigerFrame
import           TigerAssem
import           TigerErrores
import          TigerAllocation
import            TigerShow

import          Prelude as P
data Options = Options {
        optArbol     :: Bool
        ,optDebEscap :: Bool
        ,optCodInter :: Bool
        ,optEstado :: Bool
    }
    deriving Show

defaultOptions :: Options
defaultOptions = Options {optArbol = False, optDebEscap = False, optCodInter = False, optEstado = False }

options :: [OptDescr (Options -> Options)]
options = [ Option ['a'] ["arbol"] (NoArg (\opts -> opts {optArbol = True})) "Muestra el AST luego de haber realizado el cálculo de escapes"
            , Option ['e'] ["escapada"] (NoArg (\opts -> opts {optDebEscap = True})) "Stepper escapadas"
            , Option ['i'] ["intermedio"] (NoArg (\opts -> opts {optCodInter = True})) "Muestra el codigo intermedio"
            , Option ['s'] ["estado"] (NoArg (\opts -> opts {optEstado = True})) "Muestra el estado"]

compilerOptions :: [String] -> IO (Options, [String])
compilerOptions argv = case getOpt Permute options argv of
                        (o,n,[]) -> return (P.foldl (flip id) defaultOptions o, n)
                        (_,_,errs) -> ioError (userError (P.concat errs ++ usageInfo header options))
    where
        header = "Se usa: tiger fileName [OPTIONS] "

showExp :: Exp -> IO ()
showExp e = do
    putStrLn "Mostramos el AST (PP Gracias a Emilio Lopez Junior)"
    putStrLn $ renderExp e

calculoEscapadas :: Exp -> Options -> IO Exp
calculoEscapadas rawAST opts =
  if (optDebEscap opts)
  then
    either (\err ->
               putStrLn "Error de Escap:" >>
               fail (show err)
           ) (\(tree, envs) ->
                putStrLn "********* Resultado Stepper *****" >>
                mapM putStrLn (snd envs)>>
                return tree
                ) (calcularEscStepper rawAST)
  else
    either (\err ->
               putStrLn "Error de Escap:" >>
               fail (show err)
           ) return (calcularEEsc rawAST)

templabRel :: Exp -> StGen ()
templabRel ast = do
  treeS <- runSeman ast
  -- something <- canonM sometree :: StGen [Stm]
  return ()

parserStep :: Options -> String -> String -> IO Exp
parserStep opts nm sc = either
  (\perr -> error $ "Parser error" ++ show perr)
  return
  $ runParser expression () nm sc

type TigerStage = Either [Errores]

parserStage :: String -> Either [Errores] TigerAbs.Exp
parserStage s = either (\err -> Left [Error $ T.pack $ "Error de parsing:" ++ show err] ) (\exp -> Right exp) (parse s)

escapStage :: TigerAbs.Exp -> Either [Errores] TigerAbs.Exp
escapStage exp = either (\err -> Left [err]) (\exp -> Right exp)  (E.calcularEEsc exp)

translateStage :: TigerAbs.Exp -> Either [Errores] ([Frag],Integer)
translateStage exp = either (\err -> Left err) (\(frags, estado) -> Right (frags, TigerTranslate.unique estado)) (runTranslate exp)


runTranslate expre = runStateT (transProg expre :: TigerState [Frag])  initConf


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
  

main :: IO ()
main = do
    s:opts <- Env.getArgs
    (opts', _) <- compilerOptions opts
    sourceCode <- readFile s
    case tiger sourceCode of
      Left errores -> do nada <- mapM (\err -> hPutStrLn stderr $ show err)  errores
                         return ()
      Right assembly -> putStrLn assembly
    --let ret = runTranslate ast
    --case ret of
      --  Left err -> print $ show err 
--        Right (frags, estado) -> do
    
