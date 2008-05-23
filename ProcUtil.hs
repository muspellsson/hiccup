module ProcUtil (mkProc, mkLambda, procUtilTests) where

import Common
import TclErr
import ProcArgs
import Data.IORef
import Core (evalTcl)
import Control.Monad.Error
import RToken
import Util
import qualified TclObj as T

import Test.HUnit

mkLambda fn = do
        lst <- T.asList fn
        case lst of
         [al,body] -> mkProc (pack "lambda") al body
         _         -> fail "invalid lambda"


useCompiledProcs = False

mkProc pname alst body = do
  params <- parseParams pname alst
  if useCompiledProcs 
       then do
          cproc <- io $ newIORef Nothing
          count <- io $! newIORef (0 :: Int)
          return (procRunner cproc count params body)
       else return $! interpretProc params body

data CompiledProc = CompiledProc [CompCmd] T.TclObj ParamList

type TclCmd = [T.TclObj] -> TclM T.TclObj

type MRef a = (IORef (Maybe a))
data CompCmd = CompCmd (Maybe (MRef TclCmd)) Cmd

compileProc params body = do
  cmds <- asParsed body >>= mapM compCmd 
  fail "no compiler. sorry"
  return (CompiledProc cmds body params)

compCmd :: Cmd -> TclM CompCmd
compCmd c@(Cmd (BasicCmd _) _) = do
       r <- io $ newIORef Nothing
       return $ CompCmd (Just r) c
compCmd c = return $ CompCmd Nothing c

runCompiled (CompiledProc _ o a) args = interpretProc a o args 

cMAX_ATTEMPTS = 3

procRunner compref attempts params body args = do
  num_attempts <- io $! readIORef attempts 
  if num_attempts > cMAX_ATTEMPTS 
    then runInterp
    else do cp <- io $ readIORef compref
            case cp of
             Nothing -> compileAndExec `orElse` (incrAttempts >> runInterp)
             Just p  -> runCompiled p args
 where runInterp = interpretProc params body args 
       incrAttempts = io $! modifyIORef attempts succ
       compileAndExec = do
         cp <- compileProc params body
         io $ writeIORef compref (Just cp)
         runCompiled cp args


interpretProc pl body args = do
  locals <- bindArgs pl args
  withLocalScope locals (evalTcl body `catchError` herr)
 where herr (ERet s)  = return $! s
       herr EBreak    = tclErr "invoked \"break\" outside of a loop"
       herr EContinue = tclErr "invoked \"continue\" outside of a loop"
       herr e         = throwError e


procUtilTests = TestList [] 
