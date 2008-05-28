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


procCatcher f = f `catchError` herr
 where herr (ERet s)  = return $! s
       herr EBreak    = tclErr "invoked \"break\" outside of a loop"
       herr EContinue = tclErr "invoked \"continue\" outside of a loop"
       herr e         = throwError e

runCompiled (CProc cb pl) args = do
  locals <- bindArgs pl args
  withLocalScope locals (procCatcher (runCodeBlock cb))
