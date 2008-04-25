module Expr (runAsExpr, CBData(..), exprTests) where

import Test.HUnit
import BSExpr (Exprable(..), bsExprTests)
import Expr.Eval (runBSExpr, CBData(..), exprEvalTests)


runAsExpr o lu = asExpr o >>= \ex -> runBSExpr ex lu
{-# INLINE runAsExpr #-}

exprTests = TestList [ exprEvalTests, bsExprTests ]
