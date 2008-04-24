{-# LANGUAGE BangPatterns,OverloadedStrings #-}
module Expr.Eval (runExpr, runBSExpr, Callback, exprEvalTests) where
import Expr.TExp
import qualified TclObj as T
import VarName
import BSExpr
import Util
import qualified MathOp as Math
import qualified Data.Map as M
import Expr.Util
import Test.HUnit

objapply :: (Monad m) => Callback m -> (T.TclObj -> T.TclObj -> m T.TclObj) -> TExp -> TExp -> m T.TclObj
objapply lu f x y = do
  i1 <- runExpr x lu 
  i2 <- runExpr y lu
  f i1 i2
{-# INLINE objapply #-}

funapply lu fn al = do
  args <- mapM (\v -> runExpr v lu) al
  lu (mkCmd fn args)
 where mkCmd a b = Right (a,b)

type CBData = Either (NSQual VarName) (BString, [T.TclObj])
type Callback m = (CBData -> m T.TclObj)

getOpFun !op = case op of
    OpLt -> up Math.lessThan
    OpPlus -> Math.plus
    OpTimes -> Math.times
    OpMinus -> Math.minus
    OpDiv -> Math.divide 
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
       getVar vn = lu (Left vn)
       callFun fn args = lu (Right (fn, args))
       getItem (ANum i) = return $! T.fromInt i
       getItem (AStr s) = return $! T.fromBStr s
       getItem (AVar vn) = getVar vn
       getItem (AFun fn e) = run e >>= \r -> callFun fn [r]
       getItem (ACom _) = fail "not quite yet"

runExpr :: (Monad m) => TExp -> Callback m -> m T.TclObj
runExpr exp lu = 
  case exp of
    (TOp op a b) -> objap (getOpFun op) a b
    (TUnOp OpNot v) -> runExpr v lu >>= return . T.fromBool . not . T.asBool
    (TUnOp OpNeg v) -> runExpr v lu >>= procNegate
    (TVal v) -> return $! v
    (TVar n) -> lu (Left (parseVarName n))
    (TFun fn al) -> funapply lu fn al
 where objap = objapply lu

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
     where eql a b = (runExpr a (return . make)) ~=? Just b
           make (Left _)  = T.fromBStr "ERROR"
           make (Right _) = T.fromStr "PROC"
     
    varEvalTests = TestList [
        "$num -> 4" ~: (TVar "num") `eql` (mint 4),
        ((TVar "num") + (tInt 3)) `eql` (mint 7),
        ((tInt 4) + ((TVar "num") - (tInt 1))) `eql` (mint 7),
        "$boo == \"bean\" -> true" ~: ((TVar "boo") `eq` (tStr "bean")) `eql` T.tclTrue
      ]
     where eql a b = (runExpr a lu) ~=? Just b
           table = M.fromList . mapFst pack $ [("boo", T.fromStr "bean"), ("num", T.fromInt 4)]
           lu :: (Monad m) => Callback m
           lu (Left (NSQual _ (VarName v _)))  = M.lookup v table
           lu (Right _) = return $ T.fromStr "PROC"
