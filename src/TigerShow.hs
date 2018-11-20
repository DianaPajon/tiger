module TigerShow where

import TigerEmit
import TigerTemp
import Text.Regex
import Data.Text as T
import Prelude as P
type Coloreador = Temp -> Temp

naiveColorer :: Coloreador
naiveColorer = id


applyDests :: Coloreador -> [Temp] -> String  -> String
applyDests colores registros instruccion = applyTemps "`d" colores registros instruccion 0

applySrcs :: Coloreador -> [Temp] -> String  -> String
applySrcs colores registros instruccion = applyTemps "`s" colores registros instruccion 0

applyTemps :: String -> Coloreador -> [Temp] -> String  -> Integer -> String
applyTemps prefijo colores [] instruccion n = instruccion
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