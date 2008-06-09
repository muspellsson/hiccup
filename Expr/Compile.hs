{-# LANGUAGE BangPatterns,OverloadedStrings #-}
module Expr.Compile (
   compileExpr,
   getUnFun,
   getOpFun )  where

import qualified MathOp as Math
import qualified TObj as T
import Expr.TExp

compileExpr fwr = comp
 where comp e = case e of
                Item v        -> CItem v
                DepItem (DFun f ex) -> DItem (DFun f (map comp ex))
                DepItem (DVar vn)   -> DItem (DVar vn)
                DepItem (DCom cmd)   -> DItem (DCom (fwr cmd))
                BinApp op a b -> CApp2 op (comp a) (comp b)
                UnApp op v -> CApp op (comp v)
                TernIf a b c -> CTern (comp a) (comp b) (comp c)
                Paren e       -> comp e

getUnFun op = case op of
  OpNot -> Math.opNot
  OpNeg -> Math.opNegate

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
    OpLShift -> Math.leftShift
    OpRShift -> Math.rightShift
    OpIn     -> Math.opIn
 where up f a b = return (f a b)
       sup f a b = return (T.fromBool (f a b))

procBool f a b = do 
   let ab = T.asBool a
   let bb = T.asBool b
   return $! T.fromBool (ab `f` bb)
