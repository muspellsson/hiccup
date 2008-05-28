module Proc.Compiled where
import TclErr
import Control.Monad.Error
import Common
import Proc.Params
import Proc.CodeBlock

data CompiledProc = CProc CodeBlock ParamList

compileProc params body = do
  cb <- toCodeBlock body
  return (CProc cb params)


runCompiled (CProc cb pl) args = do
  locals <- bindArgs pl args
  withLocalScope locals (procCatcher (runCodeBlock cb))

procCatcher f = f `catchError` herr
 where herr (ErrTramp e) = throwError e 
       herr e = case toEnum (errCode e) of 
                   EReturn   -> return $! (errData e)
                   EBreak    -> tclErr "invoked \"break\" outside of a loop"
                   EContinue -> tclErr "invoked \"continue\" outside of a loop"
                   _         -> throwError e

{-# INLINE procCatcher #-}
