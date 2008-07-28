module Tests where
import BSParse (bsParseTests)
import TclParse (tclParseTests)
import TclObj (tclObjTests)
import Common (commonTests)
import TclLib.StringCmds (stringTests)
import TclLib.MathProcs (mathTests)
import TclLib
import Core (coreTests)
import RToken (rtokenTests)
import Util (utilTests)
import VarName (varNameTests)
import Expr (exprTests)
import Proc.Util (procUtilTests)
import ArgParse (argParseTests)
import Test.HUnit 

allTests = TestList [ bsParseTests, tclParseTests, utilTests, tclObjTests, commonTests, 
                      coreTests, rtokenTests, stringTests, mathTests, varNameTests, exprTests,
                      argParseTests
                      ]
runUnit = runTestTT allTests

main = runUnit
