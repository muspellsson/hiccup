module Expr.TExp where

import qualified TclObj as T
import Util (BString)

data Op = OpDiv | OpPlus | OpMinus | OpTimes | OpEql | OpNeql |
          OpLt | OpGt | OpLte | OpGte | OpStrNe | OpStrEq | OpAnd |
          OpOr
  deriving (Show,Eq,Ord)

data UnOp = OpNot | OpNeg deriving (Eq,Show)

data TExp = TOp !Op TExp TExp | TUnOp UnOp TExp | TVar BString 
            | TFun BString [TExp] | TVal T.TclObj 
   deriving (Show,Eq)
