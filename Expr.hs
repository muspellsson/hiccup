module Expr (runAsExpr, runAsBsExpr, exprTests) where

import Test.HUnit
import Expr.Parse (expr, exprParseTests)
import BSExpr (Exprable(..))
import Expr.Eval (runExpr, runBSExpr, exprEvalTests)

runAsExpr s f = expr s >>= \e -> runExpr e f

runAsBsExpr o lu = asExpr o >>= \ex -> runBSExpr ex lu

exprTests = TestList [ exprParseTests, exprEvalTests ]
