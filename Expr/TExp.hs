{-# OPTIONS_GHC -XMultiParamTypeClasses #-}
module Expr.TExp where

import TclParse (SubCmd)
import VarName
import Util

data TNum = TInt !Int | TDouble !Double deriving (Show,Eq)

data Atom = AStr !BString | ABlock !BString | ANum !TNum 
             deriving (Eq,Show)

data Dep c a = DCom c
         | DFun !BString [a]
         | DVar !(NSQual VarName) deriving (Eq,Show)

data Expr = Item Atom 
          | BinApp !Op Expr Expr  
          | UnApp !UnOp Expr 
          | TernIf Expr Expr Expr
          | DepItem (Dep SubCmd Expr)
          | Paren Expr deriving (Eq,Show)

data Op = OpDiv | OpPlus | OpMinus | OpTimes | OpEql | OpNeql |
          OpLt | OpGt | OpLte | OpGte | OpStrNe | OpStrEq | OpAnd |
          OpOr | OpExp | OpLShift | OpRShift | OpIn
  deriving (Show,Eq,Ord,Enum, Bounded)

data UnOp = OpNot | OpNeg deriving (Eq,Show)

data CExpr c t = CApp2 !Op (CExpr c t) (CExpr c t) | CItem !Atom 
               | CApp !UnOp (CExpr c t)
               | CStrTok t
               | CTern (CExpr c t) (CExpr c t) (CExpr c t)
               | DItem (Dep c (CExpr c t)) deriving (Eq,Show)


class Exprable e ce where
  asCExpr :: (Monad m) => e -> m ce
