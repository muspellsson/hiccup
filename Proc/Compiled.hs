module Proc.Compiled (compileProc, runCompiled) where
import Control.Monad.Error
import Proc.Params
import Proc.CodeBlock

data CompiledProc = CProc CodeBlock ParamList

compileProc params body = do
  cb <- toCodeBlock body
  return (CProc cb params)

runCompiled (CProc cb _) = runCodeBlock cb

