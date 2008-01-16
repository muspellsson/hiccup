module Tests where
import BSParse (bsParseTests)
import Hiccup (hiccupTests)
import TclObj (tclObjTests)
import Common (commonTests)
import StringProcs (stringTests)
import Core (coreTests)
import RToken (rtokenTests)
import Test.HUnit  

allTests = TestList [ bsParseTests, hiccupTests, tclObjTests, commonTests, coreTests, rtokenTests, stringTests ]
runUnit = runTestTT allTests

main = runUnit
