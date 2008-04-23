module Expr.Eval (runExpr,Callback, exprEvalTests) where
import Expr.TExp
import qualified TclObj as T
import Util
import qualified TclLib.MathProcs as Math
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

type CBData = Either BString (BString, [T.TclObj])
type Callback m = (CBData -> m T.TclObj)

mkCmd a b = Right (pack a,b)

runExpr :: (Monad m) => TExp -> Callback m -> m T.TclObj
runExpr exp lu = 
  case exp of
    (TOp OpPlus a b) -> objap Math.plus a b
    (TOp OpTimes a b) -> objap Math.times a b
    (TOp OpMinus a b) -> objap Math.minus a b
    (TOp OpDiv a b) -> objap Math.divide a b
    (TOp OpEql a b) -> objap (up Math.equals) a b
    (TOp OpLt a b) -> objap (up Math.lessThan) a b
    (TOp OpNeql a b) -> objap (up Math.notEquals) a b
    (TOp OpGt a b) -> objap (up Math.greaterThan) a b
    (TOp OpLte a b) -> objap (up Math.lessThanEq) a b
    (TOp OpGte a b) -> objap (up Math.greaterThanEq) a b
    (TOp OpStrEq a b) -> objap (sup T.strEq) a b
    (TOp OpStrNe a b) -> objap (sup T.strNe) a b
    (TOp OpAnd a b) -> objap (procBool (&&)) a b
    (TOp OpOr a b) -> objap (procBool (||)) a b
    (TUnOp OpNot v) -> runExpr v lu >>= return . T.fromBool . not . T.asBool
    (TVal v) -> return $! v
    (TVar n) -> lu (Left (pack n))
    (TFun fn al)  -> funapply lu fn al
 where objap = objapply lu
       up f a b = return (f a b)
       sup f a b = return (T.fromBool (f a b))

procBool f a b = do 
   let ab = T.asBool a
   let bb = T.asBool b
   return $! T.fromBool (ab `f` bb)

exprEvalTests = TestList [evalTests, varEvalTests] where
    mint v = T.mkTclInt v
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
           make (Left a)  = T.mkTclBStr a
           make (Right _) = T.mkTclStr "PROC"
     
    varEvalTests = TestList [
        "$num -> 4" ~: (TVar "num") `eql` (mint 4),
        ((TVar "num") + (tInt 3)) `eql` (mint 7),
        ((tInt 4) + ((TVar "num") - (tInt 1))) `eql` (mint 7),
        "$boo == \"bean\" -> true" ~: ((TVar "boo") `eq` (tStr "bean")) `eql` T.tclTrue
      ]
     where eql a b = (runExpr a lu) ~=? Just b
           table = M.fromList . mapFst pack $ [("boo", T.mkTclStr "bean"), ("num", T.mkTclInt 4)]
           lu :: (Monad m) => Callback m
           lu (Left v)  = M.lookup v table
           lu (Right _) = return $ T.mkTclStr "PROC"
