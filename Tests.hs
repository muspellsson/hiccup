module Tests where
import BSParse (bsParseTests)
import Hiccup (hiccupTests)
import TclObj (tclObjTests)
import Test.HUnit  -- IGNORE

allTests = TestList [ bsParseTests, hiccupTests, tclObjTests ]
runUnit = runTestTT allTests

main = runUnit
