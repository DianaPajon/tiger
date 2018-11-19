module Tools where

import           System.Console.ANSI
import           System.Directory

-- Pequeña librería para generar suit de pruebas para el compilador.
-- En gral cada |String| que veamos es en realidad un PATHFILE.

-- Definimos una carpeta donde podemos tirar casos que sabemos que están bien,
-- en __todas__ las etapas! Y por ende son utiles para tener a mano como |good_loc|
good_loc :: String
good_loc = "./test/test_code/good"

-- | These ones should fail
-- Definimos una carpeta donde podemos tirar casos que sabemos que están mal...
-- Ojo que pueden ser errores que fallen más adelante... así que a tener cuidado.
-- Acá son errores de sintaxis y por ende deben fallar al principio... Utiles para
-- tener una especie de test de regresión.
bad_loc :: String
bad_loc = "./test/test_code/syntax"

-- Otra carpeta donde hay errores Semánticos y errores de variables no definidas.
-- Esto quiere decir que en el test de Escap deben fallar en algunos casos
-- pero no en todos. Y en Seman sí deberían fallar. Inclusive los que fallaban
-- para Escap.
type_loc :: String
type_loc = "./test/test_code/type"


-- | Función general para testear.
-- Toma un FilePath en forma de |String|
test :: String
  -- | Una función para ejecutar en caso de que falle
  -> (a -> IO ())
  -- | Una función para ejecutar en caso de que termine correctamente.
  -> (b -> IO ())
  -- | Función que realiza el test propiamente dicho, ejecuta algo.
  -> (String -> Either a b)
  -- | Nombre del archivo. De forma tal que la ruta sea el FilePath/Nombre
  -> String
  ->  IO ()
test loc bad good f s = readFile (loc ++ '/' : s) >>=
                        either (\a -> print ("Filename:" ++ s) >> bad a) good . f

-- | Simplificamos |test| asumiendo que a izquierda es un error y usamos
-- |badRes| que muestra un mensaje en rojo, y a derecha es correcto y mostramos un
-- mensaje en azul.
testGood :: Show a => String -> (String -> Either a b) -> String -> IO ()
testGood loc = test loc ( badRes . show )
                        ( const bluenice )

-- | Acá asumimos que los test deben fallar, y por ende detectar la falla es
-- algo positivo y lo mostramos en azul.
testBad loc  = test loc (const bluefail )
               (const rednice )

-- | Esto testea permite testear archivos en la carpeta |good_loc|
testSTDGood :: Show a => (String -> Either a b) -> String -> IO ()
testSTDGood = testGood good_loc

-- | Esto testea permite testear archivos en la carpeta |bad_loc|
testSTDBad :: (String -> Either a b) -> String -> IO ()
testSTDBad = testBad bad_loc

-- | Pequeño helper para testear toda una carpeta usando un tester dado.
testDir :: String -> (String -> IO ()) -> IO ()
testDir dir tester = (listDirectory dir >>= mapM_ tester)

--------------------------------------------------------------------------------
--  Configuración de colores y pavadas para mostrar mensajes significativos
--------------------------------------------------------------------------------

setRed = setSGR [SetColor Foreground Vivid Red]
setBlue = setSGR [SetColor Foreground Vivid Blue]
reset = setSGR []

colorPrint c s = c >> putStrLn s >> reset

redfail :: IO()
redfail = colorPrint setRed "FAIL"

bluefail :: IO()
bluefail = colorPrint setBlue "Nice Fail!"

bluenice :: IO()
bluenice = colorPrint setBlue "NICE!"

rednice :: IO()
rednice = colorPrint setRed "BAD NICE!"


goodRes :: String -> IO ()
goodRes = colorPrint setBlue

badRes :: String -> IO ()
badRes = colorPrint setRed

badResult :: Show a => String -> a -> IO ()
badResult nm err = badRes nm >> print err
--------------------------------------------------------------------------------
