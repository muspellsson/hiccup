module TclLib.Interp (mkInterp, mkInterpWithVars, runInterp, interpCmds) where

import Common
import Util
import Data.IORef
import TclErr
import qualified TclObj as T
import TclLib.LibUtil
import TclLib (libCmds)
import Core (evalTcl)

import Data.Unique


interpCmds = makeCmdList [
    ("interp", cmdInterp)    
  ]

cmdInterp = mkEnsemble "interp" [
    ("create", interp_create)
    ,("eval", interp_eval)
    ,("issafe", interp_issafe)
    ,("exists", interp_exists)
    ,("delete", interp_delete)
  ]

interp_exists :: [T.TclObj] -> TclM T.TclObj
interp_exists args = case args of
    [n] -> (getInterp (T.asBStr n) >> return (T.fromBool True)) `orElse` (return $ T.fromBool False)
    _   -> argErr "interp exists"

-- delete ?path ?...
interp_delete args = case args of
    [n] -> do 
        deleteInterp (T.asBStr n)
        renameCmd (T.asBStr n) (pack "")
        ret
    _   -> argErr "interp delete"

interp_issafe args = case args of
  []  -> return (T.fromBool False)
  [n] -> do
     getInterp (T.asBStr n)
     return (T.fromBool False)
  _   -> argErr "interp issafe"


allCmds = mergeCmdLists [interpCmds, libCmds]

uniqueName = do
   i <- newUnique >>= return . hashUnique
   return . pack $ "interp" ++ show i

interp_create args = case args of
    [] -> io uniqueName >>= create . T.fromBStr
    [n] -> create n
    _   -> argErr "interp create"
 where create n = do 
           let bsn = T.asBStr n
           ir <- createInterp bsn allCmds 
           registerProc bsn (pack "NO.") (interpEnsem n ir)
           return n

interpEnsem n ir = 
  mkEnsemble (T.asStr n) [("eval", interpEval ir)]

interp_eval args = case args of
   (n:xs) -> do
        it <- getInterp (T.asBStr n)
        interpEval it xs
   _      -> argErr "interp eval"

interpEval ir cmds = do
   res <- io $ runInterp' (evalTcl (T.objconcat cmds)) (Interpreter ir)
   case res of
     Left e -> tclErr (unpack e)
     Right v -> return (T.fromBStr v)

data Interpreter = Interpreter (IORef TclState)

mkInterp = mkInterpWithVars []
mkInterpWithVars vars cmds = do
              st <- makeState vars cmds
              stref <- newIORef st
              return (Interpreter stref)

runInterp :: BString -> Interpreter -> IO (Either BString BString)
runInterp s = runInterp' (evalTcl ((T.fromBStr s) :: T.TclObj))

runInterp' t (Interpreter i) = do
                 bEnv <- readIORef i
                 (r,i') <- runTclM t bEnv
                 writeIORef i i'
                 return (fixErr r)
  where perr e = case toEnum (errCode e) of
                   EError  -> Left $ T.asBStr (errData e)
                   EOk     ->  Right $ T.asBStr (errData e)
                   EReturn ->  Right $ T.asBStr (errData e)
                   EBreak  ->  Left . pack $ "invoked \"break\" outside of a loop"
                   EContinue -> Left . pack $ "invoked \"continue\" outside of a loop"
        fixErr (Left x)  = perr x
        fixErr (Right v) = Right (T.asBStr v)
