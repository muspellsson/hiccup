module Tests where
import BSParse (bsParseTests)
import Hiccup (hiccupTests)
import TclObj (tclObjTests)
import Common (commonTests)
import Core (coreTests)
import RToken (rtokenTests)
import Test.HUnit  -- IGNORE

allTests = TestList [ bsParseTests, hiccupTests, tclObjTests, commonTests, coreTests, rtokenTests ]
runUnit = runTestTT allTests

main = runUnit
