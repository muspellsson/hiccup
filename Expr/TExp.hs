module Expr.TExp where

import TclParse (TokCmd)
import VarName
import Util


data TNum = TInt !Int | TDouble !Double deriving (Show,Eq)

data Atom = AStr !BString | ANum !TNum | AVar !(NSQual VarName)
           | AFun !BString Expr | ACom TokCmd deriving (Eq,Show)

data Expr = Item Atom 
          | BinApp !Op Expr Expr  
          | UnApp !UnOp Expr 
          | Paren Expr deriving (Eq,Show)

data Op = OpDiv | OpPlus | OpMinus | OpTimes | OpEql | OpNeql |
          OpLt | OpGt | OpLte | OpGte | OpStrNe | OpStrEq | OpAnd |
          OpOr | OpExp
  deriving (Show,Eq,Ord,Enum, Bounded)

data UnOp = OpNot | OpNeg deriving (Eq,Show)

