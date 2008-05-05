{-# LANGUAGE BangPatterns,OverloadedStrings #-}
module Expr.Compile where

import qualified MathOp as Math
import qualified TObj as T
import Expr.TExp

compileExpr :: (Monad m, T.ITObj t) => Expr -> CExpr t m
compileExpr = comp
 where comp e = case e of
                Item v        -> CItem v
                DepItem (DFun f ex) -> DItem (DFun f (compileExpr ex))
                DepItem (DVar vn)   -> DItem (DVar vn)
                DepItem (DCom cmd)   -> DItem (DCom cmd)
                BinApp op a b -> CApp2 (getOpFun op) (comp a) (comp b)
                UnApp op v -> CApp (getUnFun op) (comp v)
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
 where up f a b = return (f a b)
       sup f a b = return (T.fromBool (f a b))

procBool f a b = do 
   let ab = T.asBool a
   let bb = T.asBool b
   return $! T.fromBool (ab `f` bb)
