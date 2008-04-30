{-# LANGUAGE BangPatterns #-}
module Hiccup (runTcl, runTclWithArgs, mkInterp, runInterp, hiccupTests) where

import Control.Monad.Error
import Data.IORef

import Util
import qualified TclObj as T
import Core (evalTcl)
import Common
import ProcArgs

import TclLib (libCmds)

import Test.HUnit 

coreProcs = makeCmdList $
 [("proc", procProc),
  ("break", procRetv EBreak),
  ("continue", procRetv EContinue)]


baseCmds = mergeCmdLists [libCmds, coreProcs]
                          
processArgs al = [("argc" * T.mkTclInt (length al)), ("argv" * T.mkTclList al)]
  where (*) name val = (pack name, val)

interpVars = [("tcl_version" * (show hiccupVersion))]
  where (*) name val = (pack name, T.mkTclStr val)

hiccupVersion = 0.45

data Interpreter = Interpreter (IORef TclState)

mkInterp = mkInterpWithArgs []

mkInterpWithArgs :: [BString] -> IO Interpreter
mkInterpWithArgs args = do
              st <- makeState (interpVars ++ (processArgs (map T.mkTclBStr args))) baseCmds
              stref <- newIORef st
              return (Interpreter stref)


runInterp :: BString -> Interpreter -> IO (Either BString BString)
runInterp s = runInterp' (evalTcl (T.mkTclBStr s))

runInterp' t (Interpreter i) = do
                 bEnv <- readIORef i
                 (r,i') <- runTclM t bEnv
                 writeIORef i i'
                 return (fixErr r)
  where perr (EDie s)    = Left $ pack s
        perr (ERet v)    = Right $ T.asBStr v
        perr EBreak      = Left . pack $ "invoked \"break\" outside of a loop"
        perr EContinue   = Left . pack $ "invoked \"continue\" outside of a loop"
        fixErr (Left x)  = perr x
        fixErr (Right v) = Right (T.asBStr v)

runTcl v = mkInterp >>= runInterp v
runTclWithArgs v args = mkInterpWithArgs args >>= runInterp v

procRetv c args = case args of
    [] -> throwError c
    _  -> argErr $ st c
 where st EContinue = "continue"
       st EBreak    = "break"
       st _         = "??"

procProc args = case args of
  [name,alst,body] -> do
    let pname = T.asBStr name
    params <- parseParams pname alst
    proc <- mkProc params body
    registerProc pname (T.asBStr body) proc
    ret
  _               -> argErr "proc"

mkProc params body = do
  return (procRunner params body)

procRunner pl body args = do
  locals <- bindArgs pl args
  withLocalScope locals (evalTcl body `catchError` herr)
 where herr (ERet s)  = return $! s
       herr EBreak    = tclErr "invoked \"break\" outside of a loop"
       herr EContinue = tclErr "invoked \"continue\" outside of a loop"
       herr e         = throwError e

hiccupTests = TestList []
