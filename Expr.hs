module Expr (runAsExpr, CBData(..), exprTests) where

import Test.HUnit
import Expr.Parse (bsExprTests)
import Expr.Eval (runExpr, runCExpr, CBData(..), exprEvalTests)
import Expr.TExp (asCExpr)


runAsExpr o lu = asCExpr o >>= runCExpr lu
{-# INLINE runAsExpr #-}

exprTests = TestList [ exprEvalTests, bsExprTests ]
