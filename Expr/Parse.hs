{-# LANGUAGE BangPatterns,OverloadedStrings #-}
module Expr.Parse  where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr

import qualified TclObj as T
import Util (pack)

import Expr.TExp
import Expr.Util

import Test.HUnit

import Util 

data TExp = TOp !Op TExp TExp | TUnOp UnOp TExp | TVar BString 
            | TFun BString [TExp] | TVal T.TclObj 
   deriving (Show,Eq)


