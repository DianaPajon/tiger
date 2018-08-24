import           Text.Parsec
import           TigerParser

import Tools

main :: IO ()
main =
  putStrLn "\n======= Test PARSER in progress =======" >>
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

tester s = runParser expression () s s
