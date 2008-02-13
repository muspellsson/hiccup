module Tests where
import BSParse (bsParseTests)
import Hiccup (hiccupTests)
import TclObj (tclObjTests)
import Common (commonTests)
import TclLib.StringProcs (stringTests)
import Core (coreTests)
import RToken (rtokenTests)
import Util (utilTests)
import VarName (varNameTests)
import ExprParse (exprParseTests)
import Test.HUnit 

allTests = TestList [ bsParseTests, utilTests, hiccupTests, tclObjTests, commonTests, 
                      coreTests, rtokenTests, stringTests, varNameTests, exprParseTests ]
runUnit = runTestTT allTests

main = runUnit
