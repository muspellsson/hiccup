{-# OPTIONS_GHC -XMultiParamTypeClasses #-}
module Expr.TExp where

import TclParse (TokCmd)
import VarName
import Util


data TNum = TInt !Int | TDouble !Double deriving (Show,Eq)

data Atom = AStr !BString | ANum !TNum 
             deriving (Eq,Show)

data Dep c a = DCom c
         | DFun !BString a
         | DVar !(NSQual VarName) deriving (Eq,Show)

data Expr = Item Atom 
          | BinApp !Op Expr Expr  
          | UnApp !UnOp Expr 
          | DepItem (Dep TokCmd Expr)
          | Paren Expr deriving (Eq,Show)

data Op = OpDiv | OpPlus | OpMinus | OpTimes | OpEql | OpNeql |
          OpLt | OpGt | OpLte | OpGte | OpStrNe | OpStrEq | OpAnd |
          OpOr | OpExp | OpLShift | OpRShift
  deriving (Show,Eq,Ord,Enum, Bounded)

data UnOp = OpNot | OpNeg deriving (Eq,Show)

data CExpr c = CApp2 !Op (CExpr c) (CExpr c) | CItem Atom 
               | CApp !UnOp (CExpr c)
               | DItem (Dep c (CExpr c)) deriving (Eq,Show)


class Exprable e c where
  asCExpr :: (Monad m) => e -> m (CExpr c)
