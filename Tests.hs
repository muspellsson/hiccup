module Tests where
import BSParse (bsParseTests)
import Hiccup (hiccupTests)
import TclObj (tclObjTests)
import Common (commonTests)
import TclLib.StringProcs (stringTests)
import TclLib.MathProcs (mathTests)
import Core (coreTests)
import RToken (rtokenTests)
import Util (utilTests)
import VarName (varNameTests)
import Expr (exprTests)
import Test.HUnit 

allTests = TestList [ bsParseTests, utilTests, hiccupTests, tclObjTests, commonTests, 
                      coreTests, rtokenTests, stringTests, mathTests, varNameTests, exprTests ]
runUnit = runTestTT allTests

main = runUnit
