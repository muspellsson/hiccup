module Expr.TExp where

import qualified TclObj as T

data Op = OpDiv | OpPlus | OpMinus | OpTimes | OpEql | OpNeql |
          OpLt | OpGt | OpLte | OpGte | OpStrNe | OpStrEq | OpAnd |
          OpOr
  deriving (Show,Eq)

data UnOp = OpNot deriving (Eq,Show)

data TExp = TOp !Op TExp TExp | TUnOp UnOp TExp | TVar String 
            | TFun String [TExp] | TVal T.TclObj 
   deriving (Show,Eq)
