module TigerShow (makeProgram,printCode) where

import TigerEmit
import TigerTemp
import Text.Regex
import TigerFrame (Frag(AString))
import Data.Text as T
import Prelude as P
type Coloreador = Temp -> Temp

naiveColorer :: Coloreador
naiveColorer reg = T.pack ("%" ++ T.unpack reg)


applyDests :: Coloreador -> [Temp] -> String  -> String
applyDests colores registros instruccion = applyTemps "`d" colores registros instruccion 0

applySrcs :: Coloreador -> [Temp] -> String  -> String
applySrcs colores registros instruccion = applyTemps "`s" colores registros instruccion 0

applyTemps :: String -> Coloreador -> [Temp] -> String  -> Integer -> String
applyTemps prefijo colores [] instruccion n = instruccion
applyTemps prefijo colores asdf "" n = ""
applyTemps prefijo colores (t:ts) instruccion n = 
    case splitRegex (mkRegex (prefijo ++ show n)) instruccion of
        [before, after] -> applyTemps prefijo colores ts (before ++ T.unpack (colores t) ++ after) (n+1)
        [noexiste] -> noexiste
        _ -> error "Error al aplicar los registros"

printInstr  :: Coloreador -> Assem -> String
printInstr colores (Oper assembly dest src _ ) = applySrcs colores src $  applyDests colores dest assembly
printInstr colores (Mov assembly dest src) = applySrcs colores [src] $  applyDests colores [dest] assembly
printInstr colores (Lab label _) = label 

printInstrs :: Coloreador ->  [Assem] -> String
printInstrs colores  instrucciones = 
    P.foldl
        (\codigo instruccion-> codigo ++  printInstr colores  instruccion ++ "\n")
        ""
        instrucciones

printCode :: [Assem] ->  String
printCode instrucciones = printInstrs naiveColorer instrucciones

printStrFrag :: Frag -> String
printStrFrag (AString label (strings)) = unpack label ++ ":" ++ P.foldl (\declaracion linea -> declaracion ++ "\n" ++ unpack linea) "" strings ++ "\n"

makeProgram :: [[Assem]] -> [Frag] -> String
makeProgram instrucciones strings = 
    ".intel_syntax\n.data\n" ++ P.foldl (\strings string -> strings ++ "\n" ++ printStrFrag string) "" strings ++
    "\n.text\n" ++ P.foldl (\programa funcion -> programa ++ "\n" ++ printCode funcion) "" instrucciones