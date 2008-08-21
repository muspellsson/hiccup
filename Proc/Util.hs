module Proc.Util (mkProc, mkLambda, procUtilTests) where

import Common
import Proc.Compiled
import Proc.Params
import Data.IORef
import Control.Monad.Error
import TclErr
import Util
import Internal.Types (CmdCore(..))
import qualified TclObj as T

import Test.HUnit

mkLambda :: T.TclObj -> TclM ([T.TclObj] -> TclM T.TclObj)
mkLambda fn = do
        lst <- T.asList fn
        case lst of
         [al,body] -> do 
           pr <- mkProc (pack "lambda") al body 
           nsr <- getCurrNS
           return $ withProcScope (procArgs pr) nsr (procAction pr) (pack "lambda")
         _         -> fail "invalid lambda"

useCompiledProcs = True

ref = io . newIORef
readRef = io . readIORef
(.<-) r v = io $ (writeIORef r) v
(.^^) r f = io $ (modifyIORef r) f

mkProc pname alst body = do
  params <- parseParams pname alst
  if useCompiledProcs 
       then do
          cproc <- ref Nothing
          count <- ref (0 :: Int)
          let procInner = procRunner cproc count params body
          return $! ProcCore bsbody params (procCatcher procInner)
       else return $! ProcCore bsbody params (procCatcher (evalTcl body))
 where bsbody = T.asBStr body

cMAX_ATTEMPTS = 1

procRunner compref attempts params body = do
  num_attempts <- readRef attempts 
  if num_attempts > cMAX_ATTEMPTS 
    then (io $ putStrLn "reached max attempts") >> runInterp
    else do cp <- readRef compref
            case cp of
             Nothing -> compileAndExec `orElse` (incrAttempts >> runInterp)
             Just p  -> runCompiled p 
 where runInterp = evalTcl body
       incrAttempts = attempts .^^ (+ 1)
       compileAndExec = do
         cp <- compileProc params body
         compref .<- Just cp
         runCompiled cp


procCatcher f = f `catchError` herr
 where herr (ErrTramp e) = throwError e 
       herr e = case toEnum (errCode e) of 
                   EReturn   -> return $! (errData e)
                   EBreak    -> tclErr "invoked \"break\" outside of a loop"
                   EContinue -> tclErr "invoked \"continue\" outside of a loop"
                   _         -> throwError e

{-# INLINE procCatcher #-}

procUtilTests = TestList [] 
