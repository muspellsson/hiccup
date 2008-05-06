module Expr.Util where

import Expr.TExp
import Expr.Parse

tInt i = Item (ANum (TInt i))
tFloat f = Item (ANum (TDouble f))
tStr s = Item (AStr s)

(.&&) = BinApp OpAnd
(.||) = BinApp OpOr

(.<) = BinApp OpLt
(.<=) = BinApp OpLte
(.>) = BinApp OpGt
(.>=) = BinApp OpGte
(.==) = BinApp OpEql

eq = BinApp OpStrEq
ne = BinApp OpStrNe

instance Num Expr where
  a + b = BinApp OpPlus a b
  (-) = BinApp OpMinus
  (*) = BinApp OpTimes
  abs = undefined
  signum = undefined
  negate = undefined
  fromInteger i =  tInt (fromIntegral i)
