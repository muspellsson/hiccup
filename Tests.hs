module Tests where
import BSParse (bsParseTests)
import Hiccup (hiccupTests)
import TclObj (tclObjTests)
import Common (commonTests)
import StringProcs (stringTests)
import Core (coreTests)
import RToken (rtokenTests)
import Util (utilTests)
import VarName (varNameTests)
import Test.HUnit  

allTests = TestList [ bsParseTests, utilTests, hiccupTests, tclObjTests, commonTests, 
                      coreTests, rtokenTests, stringTests, varNameTests ]
runUnit = runTestTT allTests

main = runUnit
