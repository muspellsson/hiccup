module Expr (runAsExpr, exprTests) where

import Test.HUnit
import Expr.Parse (expr, exprParseTests)
import Expr.Eval (runExpr, exprEvalTests)

runAsExpr s f = expr s >>= \e -> runExpr e f

exprTests = TestList [ exprParseTests, exprEvalTests ]
