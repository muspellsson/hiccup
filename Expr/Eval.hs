{-# LANGUAGE BangPatterns,OverloadedStrings #-}
module Expr.Eval (runBSExpr, Callback, CBData(..), exprEvalTests) where
import Expr.TExp
import qualified TclObj as T
import VarName
import BSExpr
import Util
import RToken (Cmd, tokCmdToCmd)
import qualified MathOp as Math
import qualified Data.Map as M
import Expr.Util
import Test.HUnit

data CBData = VarRef (NSQual VarName) | FunRef (BString, [T.TclObj]) | CmdEval Cmd
type Callback m = (CBData -> m T.TclObj)

getOpFun !op = case op of
    OpLt -> up Math.lessThan
    OpPlus -> Math.plus
    OpTimes -> Math.times
    OpMinus -> Math.minus
    OpDiv -> Math.divide 
    OpExp -> Math.pow
    OpEql -> up Math.equals
    OpNeql -> up Math.notEquals
    OpGt -> up Math.greaterThan
    OpLte -> up Math.lessThanEq
    OpGte -> up Math.greaterThanEq
    OpStrEq -> sup T.strEq
    OpStrNe -> sup T.strNe
    OpAnd -> procBool (&&)
    OpOr -> procBool (||)
 where up f a b = return (f a b)
       sup f a b = return (T.fromBool (f a b))

runBSExpr :: (Monad m) => Expr -> Callback m -> m T.TclObj
runBSExpr exp lu = 
  case exp of
    (BinApp op a b) -> objap (getOpFun op) a b
    (UnApp OpNot v) -> run v >>= return . T.fromBool . not . T.asBool
    (UnApp OpNeg v) -> run v >>= procNegate
    (Paren e)       -> run e
    (Item v) -> getItem v
 where objap f !x !y = do
         r1 <- run x 
         r2 <- run y 
         f r1 r2
       run v = runBSExpr v lu
       getVar vn = lu (VarRef vn)
       callFun fn args = lu (FunRef (fn, args))
       getItem (ANum (TInt i)) = return $! T.fromInt i
       getItem (ANum (TDouble d)) = return $! T.fromDouble d
       getItem (AStr s) = return $! T.fromBStr s
       getItem (AVar vn) = getVar vn
       getItem (AFun fn e) = run e >>= \r -> callFun fn [r]
       getItem (ACom cmd) = lu (CmdEval (tokCmdToCmd cmd))

procNegate v = do
   i <- T.asInt v
   return $ T.fromInt (negate i)

procBool f a b = do 
   let ab = T.asBool a
   let bb = T.asBool b
   return $! T.fromBool (ab `f` bb)

exprEvalTests = TestList [evalTests, varEvalTests] where
    mint v = T.fromInt v
    evalTests = TestList
      [ 
        (tInt 3) `eql` (mint 3),
        ((tInt 5) + (tInt 5)) `eql` (mint 10),
        (((tInt 8) - (tInt 5)) + (tInt 5)) `eql` (mint 8),
        (((tInt 8) - (tInt 5)) .> (tInt 5)) `eql` T.tclFalse,
        "5 >= 5 -> true" ~: ((tInt 5) .>= (tInt 5)) `eql` (T.tclTrue),
        "5 <= 5 -> true" ~: ((tInt 5) .<= (tInt 5)) `eql` (T.tclTrue),
        ((tInt 6) .<= (tInt 5)) `eql` (T.tclFalse),
        "8 - 5 < 5 -> true" ~: (((tInt 8) - (tInt 5)) .< (tInt 5)) `eql` T.tclTrue
      ]
     where eql a b = (runBSExpr a (return . make)) ~=? Just b
           make (FunRef _) = T.fromStr "PROC"
           make _          = T.fromBStr "ERROR"
    
    var v = Item (AVar (parseVarName v))
    varEvalTests = TestList [
        "$num -> 4" ~: (var "num") `eql` (mint 4),
        ((var "num") + (tInt 3)) `eql` (mint 7),
        ((tInt 4) + ((var "num") - (tInt 1))) `eql` (mint 7),
        "$boo == \"bean\" -> true" ~: ((var "boo") `eq` (tStr "bean")) `eql` T.tclTrue
      ]
     where eql a b = (runBSExpr a lu) ~=? Just b
           table = M.fromList . mapFst pack $ [("boo", T.fromStr "bean"), ("num", T.fromInt 4)]
           lu :: (Monad m) => Callback m
           lu (VarRef (NSQual _ (VarName v _)))  = M.lookup v table
           lu (FunRef _) = return $ T.fromStr "PROC"
           lu (CmdEval _) = return $ T.fromStr "CMD"
