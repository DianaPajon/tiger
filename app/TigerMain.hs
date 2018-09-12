module Main (main) where
import           Control.Monad
import           Control.Monad.State   hiding (evalState)
import           Data.Either
import           Data.Maybe
import           System.Console.GetOpt
import qualified System.Environment    as Env
import           System.Exit

import           TigerAbs
import           TigerEscap
import           TigerParser
import           TigerPretty
import           TigerSeman
import           TigerTemp
import           TigerUnique

import           Text.Parsec           (runParser)

data Options = Options {
        optArbol     :: Bool
        ,optDebEscap :: Bool
    }
    deriving Show

defaultOptions :: Options
defaultOptions = Options {optArbol = False, optDebEscap = False }

options :: [OptDescr (Options -> Options)]
options = [ Option ['a'] ["arbol"] (NoArg (\opts -> opts {optArbol = True})) "Muestra el AST luego de haber realizado el cálculo de escapes"
            , Option ['e'] ["escapada"] (NoArg (\opts -> opts {optDebEscap = True})) "Stepper escapadas"]

compilerOptions :: [String] -> IO (Options, [String])
compilerOptions argv = case getOpt Permute options argv of
                        (o,n,[]) -> return (foldl (flip id) defaultOptions o, n)
                        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
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

main :: IO ()
main = do
    s:opts <- Env.getArgs
    (opts', _) <- compilerOptions opts
    sourceCode <- readFile s
    rawAst <- parserStep opts' s sourceCode
    ast <- calculoEscapadas rawAst opts'
    when (optArbol opts') (showExp ast)
    let _ = evalState (templabRel ast) 0
    print "Genial!"
