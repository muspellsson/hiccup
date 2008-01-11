module Tests where
import BSParse (bsParseTests)
import Hiccup (hiccupTests)
import TclObj (tclObjTests)
import Common (commonTests)
import Test.HUnit  -- IGNORE

allTests = TestList [ bsParseTests, hiccupTests, tclObjTests, commonTests ]
runUnit = runTestTT allTests

main = runUnit
