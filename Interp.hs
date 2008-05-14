module Interp where

import Common
import Util
import Data.IORef
import qualified TclObj as T
import Core (evalTcl)

data Interpreter = Interpreter (IORef TclState)

mkInterp = mkInterpWithVars []
mkInterpWithVars vars cmds = do
              st <- makeState vars cmds
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
