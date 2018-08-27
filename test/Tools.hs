module Tools where

import           System.Console.ANSI
import           System.Directory

good_loc :: String
good_loc = "./test/test_code/good"

-- | These ones should fail
bad_loc :: String
bad_loc = "./test/test_code/syntax"

type_loc :: String
type_loc = "./test/test_code/type"


test :: String -> (a -> IO ()) -> (b -> IO ()) -> (String -> Either a b) -> String ->  IO ()
test loc bad good f s = readFile (loc ++ '/' : s) >>=
                        either (\a -> print ("Filename:" ++ s) >> bad a) good . f

testSTDGood :: Show a => (String -> Either a b) -> String -> IO ()
testSTDGood = testGood good_loc

testSTDBad :: (String -> Either a b) -> String -> IO ()
testSTDBad = testBad bad_loc

testGood :: Show a => String -> (String -> Either a b) -> String -> IO ()
testGood loc = test loc ( badRes . show )
                        ( const bluenice )

testBad loc  = test loc (const bluefail )
               (const rednice )

testDir :: String -> (String -> IO ()) -> IO ()
testDir dir tester = (listDirectory dir >>= mapM_ tester)

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
