module TestMain where

import TigerPretty
import qualified TigerEscap as E
import TigerParser
import System.IO.Unsafe

main :: IO ()
main = putStrLn "\n======= Test suite Spec in progress =======" >>
       putStrLn "\n======= Test suite FIN ======="


right (Right a) = a

prettyAst dir file = 
    either
        (\err -> Left $ show err)
        (\exp -> either (\err -> Left $ show err) (\(exp,(env,lista)) -> Right $ renderExp exp) $ E.calcularEscStepper exp)
        (parse programa)
            where programa = unsafePerformIO (readFile (dir ++ '/' : file))
    