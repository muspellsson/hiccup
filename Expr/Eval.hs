{-# LANGUAGE BangPatterns,OverloadedStrings #-}
module Expr.Eval (runExpr, Callback, CBData(..), exprEvalTests) where
import Expr.TExp
import qualified TclObj as T
import VarName
import BSExpr
import Util
import qualified TObj as T
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

compileExpr :: (Monad m, T.ITObj t) => Expr -> CExpr t m
compileExpr = comp
 where comp e = case e of
                Item v        -> CItem v
                DepItem (DFun f ex) -> DItem (DFun f (compileExpr ex))
                DepItem (DVar vn)   -> DItem (DVar vn)
                DepItem (DCom cmd)   -> DItem (DCom cmd)
                BinApp op a b -> CApp2 (getOpFun op) (comp a) (comp b)
                UnApp OpNot v -> CApp opNot (comp v)
                UnApp OpNeg v -> CApp opNegate (comp v)
                Paren e       -> comp e

runCExpr lu exp = run exp
 where run e = case e of
                 CItem v -> getItem v
                 DItem v -> getDep v
                 CApp2 f a b -> do
                            va <- run a
                            vb <- run b
                            f va vb
                 CApp f a -> run a >>= f
       callFun fn args = lu (FunRef (fn, args))
       getDep item = case item of
                        DVar vn   -> lu (VarRef vn)
                        DFun fn e -> runCExpr lu e >>= \r -> callFun fn [r]
                        DCom cmd  -> lu (CmdEval (tokCmdToCmd cmd))
       getItem item = case item of
                        ANum (TInt i)    -> return $! T.fromInt i
                        ANum (TDouble d) -> return $! T.fromDouble d
                        AStr s    -> return $! T.fromBStr s


runExpr :: (Monad m) => Callback m -> Expr -> m T.TclObj
runExpr lu e = runCExpr lu (compileExpr e)

runExpr2 :: (Monad m) => Callback m -> Expr -> m T.TclObj
runExpr2 lu exp = run exp
 where run e = case e of
                Item v        -> getItem v
                DepItem v     -> getDep v
                BinApp op a b -> do 
                        va <- run a
                        vb <- run b
                        (getOpFun op) va vb
                UnApp OpNot v -> run v >>= opNot
                UnApp OpNeg v -> run v >>= opNegate
                Paren e       -> run e
       callFun fn args = lu (FunRef (fn, args))
       getDep item = case item of
                        DVar vn   -> lu (VarRef vn)
                        DFun fn e -> run e >>= \r -> callFun fn [r]
                        DCom cmd  -> lu (CmdEval (tokCmdToCmd cmd))
       getItem item = case item of
                        ANum (TInt i)    -> return $! T.fromInt i
                        ANum (TDouble d) -> return $! T.fromDouble d
                        AStr s    -> return $! T.fromBStr s

opNegate :: (Monad m, T.ITObj t) => t -> m t
opNegate v = do
   i <- T.asInt v
   return $ T.fromInt (negate i)

opNot :: (Monad m, T.ITObj t) => t -> m t
opNot = return . T.fromBool . not . T.asBool

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
     where eql a b = (runExpr (return . make) a) ~=? Just b
           make (FunRef _) = T.fromStr "PROC"
           make _          = T.fromBStr "ERROR"
    
    var v = DepItem (DVar (parseVarName v))
    varEvalTests = TestList [
        "$num -> 4" ~: (var "num") `eql` (mint 4),
        ((var "num") + (tInt 3)) `eql` (mint 7),
        ((tInt 4) + ((var "num") - (tInt 1))) `eql` (mint 7),
        "$boo == \"bean\" -> true" ~: ((var "boo") `eq` (tStr "bean")) `eql` T.tclTrue
      ]
     where eql a b = (runExpr lu a) ~=? Just b
           table = M.fromList . mapFst pack $ [("boo", T.fromStr "bean"), ("num", T.fromInt 4)]
           lu :: (Monad m) => Callback m
           lu (VarRef (NSQual _ (VarName v _)))  = M.lookup v table
           lu (FunRef _) = return $ T.fromStr "PROC"
           lu (CmdEval _) = return $ T.fromStr "CMD"
