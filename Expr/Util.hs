module Expr.Util where

import qualified TclObj as T
import Expr.TExp

tInt i = TVal (T.fromInt i)
tStr s = TVal (T.fromStr s)
tFloat f = TVal (T.fromDouble f)

(.&&) = TOp OpAnd
(.||) = TOp OpOr

(.<) = TOp OpLt
(.<=) = TOp OpLte
(.>) = TOp OpGt
(.>=) = TOp OpGte
(.==) = TOp OpEql

eq = TOp OpStrEq
ne = TOp OpStrNe

instance Num TExp where
  a + b = TOp OpPlus a b
  (-) = TOp OpMinus
  (*) = TOp OpTimes
  abs = undefined
  signum = undefined
  negate = undefined
  fromInteger i =  TVal (T.fromInt (fromIntegral i))
