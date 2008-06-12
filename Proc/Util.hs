module Proc.Util (mkProc, mkLambda, procUtilTests) where

import Common
import Proc.Compiled
import Proc.Params
import Data.IORef
import Control.Monad.Error
import TclErr
import Util
import qualified TclObj as T

import Test.HUnit

mkLambda fn = do
        lst <- T.asList fn
        case lst of
         [al,body] -> mkProc (pack "lambda") al body
         _         -> fail "invalid lambda"

useCompiledProcs = False

ref = io . newIORef
readRef = io . readIORef
(.<-) r v = io $ (writeIORef r) v

mkProc pname alst body = do
  params <- parseParams pname alst
  if useCompiledProcs 
       then do
          cproc <- ref Nothing
          count <- ref (0 :: Int)
          return (procRunner cproc count params body)
       else return $! interpretProc params body

cMAX_ATTEMPTS = 3

procRunner compref attempts params body args = do
  num_attempts <- readRef attempts 
  if num_attempts > cMAX_ATTEMPTS 
    then (io $ putStrLn "reached max attempts") >> runInterp
    else do cp <- readRef compref
            case cp of
             Nothing -> compileAndExec `orElse` (incrAttempts >> runInterp)
             Just p  -> runCompiled p args
 where runInterp = interpretProc params body args 
       incrAttempts = io $! modifyIORef attempts succ
       compileAndExec = do
         cp <- compileProc params body
         compref .<- Just cp
         runCompiled cp args


interpretProc pl body args = do
  locals <- bindArgs pl args
  withLocalScope locals (procCatcher (evalTcl body))

procUtilTests = TestList [] 
