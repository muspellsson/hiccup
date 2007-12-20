module Tests where
import BSParse (bsParseTests)
import Hiccup (hiccupTests)
import Test.HUnit  -- IGNORE

allTests = TestList [ bsParseTests, hiccupTests ]
runUnit = runTestTT allTests

main = runUnit
