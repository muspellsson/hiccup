module Proc.Compiled where
import Util
import RToken
import Common
import Proc.Params
import Data.IORef
import qualified TclObj as T

data CompiledProc = CProc [CompCmd] T.TclObj ParamList

type TclCmd = [T.TclObj] -> TclM T.TclObj

type MRef a = (IORef (Maybe a))
data CompCmd = CompCmd (Maybe (MRef TclCmd)) Cmd

compileProc params body = do
  cmds <- asParsed body >>= mapM compCmd 
  fail "no compiler. sorry"
  return (CProc cmds body params)

compCmd :: Cmd -> TclM CompCmd
compCmd c@(Cmd (BasicCmd _) _) = do
       r <- io $ newIORef Nothing
       return $ CompCmd (Just r) c
compCmd c = return $ CompCmd Nothing c

runCompiled (CProc _ o a) args = fail "no can do"
