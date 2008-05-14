{-# LANGUAGE BangPatterns #-}
module Hiccup (runTcl, runTclWithArgs, mkMainInterp, runInterp, hiccupTests) where

import Interp
import ProcUtil
import Control.Monad.Error

import Util
import qualified TclObj as T
import Common

import TclLib (libCmds)

import Test.HUnit 

coreProcs = makeCmdList $
 [("proc", procProc),
  ("break", procRetv EBreak),
  ("apply", cmdApply),
  ("continue", procRetv EContinue)]


baseCmds = mergeCmdLists [libCmds, coreProcs]
                          
processArgs al = [("argc" * T.fromInt (length al)), ("argv" * T.mkTclList al)]
  where (*) name val = (pack name, val)

interpVars = [("tcl_version" * (show hiccupVersion))]
  where (*) name val = (pack name, T.fromStr val)

hiccupVersion = 0.48

mkMainInterp = mkInterp baseCmds

runTcl v = mkMainInterp >>= runInterp v
runTclWithArgs v args = mkInterpWithVars mainVars baseCmds >>= runInterp v
 where mainVars = interpVars ++ (processArgs (map T.fromBStr args))

procRetv c args = case args of
    [] -> throwError c
    _  -> argErr $ st c
 where st EContinue = "continue"
       st EBreak    = "break"
       st _         = "??"

cmdApply args = case args of
   (fn:alst) -> mkLambda fn >>= \f -> f alst
   _         -> argErr "apply"

procProc args = case args of
  [name,alst,body] -> do
    let pname = T.asBStr name
    proc <- mkProc pname alst body
    registerProc pname (T.asBStr body) proc
    ret
  _               -> argErr "proc"


hiccupTests = TestList []
