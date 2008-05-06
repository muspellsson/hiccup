module Expr (runAsExpr, CBData(..), exprTests) where

import Test.HUnit
import Expr.Parse (bsExprTests)
import Expr.Eval (runExpr, CBData(..), exprEvalTests)
import Expr.TExp (asExpr)


runAsExpr o lu = asExpr o >>= runExpr lu
{-# INLINE runAsExpr #-}

exprTests = TestList [ exprEvalTests, bsExprTests ]
