module Expr (runAsExpr, exprTests) where


import ExprParse (expr, exprParseTests)
import Expr.Eval (runExpr, exprEvalTests)
import Test.HUnit

runAsExpr s f = expr s >>= \e -> runExpr e f

exprTests = TestList [ exprParseTests, exprEvalTests ]
