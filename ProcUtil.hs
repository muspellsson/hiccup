module ProcUtil (mkProc, mkLambda) where

import Common
import TclErr
import ProcArgs
import Core (evalTcl)
import Control.Monad.Error
import Util
import qualified TclObj as T

mkProc pname alst body = do
  params <- parseParams pname alst
  return (procRunner params body)

procRunner pl body args = do
  locals <- bindArgs pl args
  withLocalScope locals (evalTcl body `catchError` herr)
 where herr (ERet s)  = return $! s
       herr EBreak    = tclErr "invoked \"break\" outside of a loop"
       herr EContinue = tclErr "invoked \"continue\" outside of a loop"
       herr e         = throwError e

mkLambda fn = do
        lst <- T.asList fn
        case lst of
         [al,body] -> mkProc (pack "lambda") al body
         _         -> fail "invalid lambda"
