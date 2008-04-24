module Expr.TExp where

data Op = OpDiv | OpPlus | OpMinus | OpTimes | OpEql | OpNeql |
          OpLt | OpGt | OpLte | OpGte | OpStrNe | OpStrEq | OpAnd |
          OpOr
  deriving (Show,Eq,Ord,Enum, Bounded)

data UnOp = OpNot | OpNeg deriving (Eq,Show)

