module TclLib.Interp (mkInterp, mkInterpWithVars, interpEvalStr, interpCmds) where

import Common
import Util
import Control.Monad.Error (throwError)
import Data.IORef
import TclErr
import qualified TclObj as T
import TclLib.LibUtil
import TclLib (libCmds)
import Core ()
import ArgParse
import Internal.Types (Interp(..),interpSafe)
import CmdList 

import Data.Unique


interpCmds = makeCmdList [
    ("interp", cmdInterp)    
  ]

cmdInterp = mkEnsemble "interp" [
    ("create", interp_create)
    ,("eval", interp_eval)
    ,("issafe", interp_issafe)
    ,("exists", interp_exists)
    ,("slaves", interp_slaves)
    ,("delete", interp_delete)
  ]

toPath i = T.asList i >>= return . map T.asBStr

interp_exists :: [T.TclObj] -> TclM T.TclObj
interp_exists args = case args of
    [n] -> do 
     path <- toPath n
     (getInterp path >> return (T.fromBool True)) `orElse` (return $ T.fromBool False)
    _   -> argErr "interp exists"

interp_slaves args = case args of
   [] -> getInterps >>= return . T.fromList . map T.fromBStr
   _  -> vArgErr "interp slaves ?path?"

-- delete ?path ?...
interp_delete args = case args of
    [n] -> do 
        path <- toPath n
        deleteInterp path
        ret
    _   -> argErr "interp delete"

interp_issafe args = case args of
  []  -> issafe []
  [n] -> toPath n >>= issafe
  _   -> argErr "interp issafe"
 where issafe lst = do
         ir <- getInterp lst >>= io . readIORef . interpState
         return (T.fromBool (interpSafe ir))


allCmds = mergeCmdLists [interpCmds, libCmds]

uniqueName = do
   i <- newUnique >>= return . hashUnique
   return . pack $ "interp" ++ show i

safeFlag = boolFlagSpec "safe" 1

interp_create args_ = do
   (safe,args) <- parseArgs safeFlag False args_
   case args of
    [] -> io uniqueName >>= create safe . T.fromBStr
    [n] -> create safe n
    _   -> argErr "interp create"
 where create safe n = do 
           path <- toPath n
           ir <- io $ createInterp safe [] allCmds 
           registerInterp path ir (interpEnsem n ir)
           return n

interpEnsem n ir = mkEnsemble (T.asStr n) [("eval", interpEval ir)]

interp_eval args = case args of
   (n:xs) -> do
        it <- toPath n >>= getInterp 
        interpEval it xs
   _      -> argErr "interp eval"

interpEval ir cmds = do
   res <- io $ runInterp (evalTcl (T.objconcat cmds)) ir
   case res of
     Left e -> case toEnum (errCode e) of
        EOk -> return (errData e)
        EReturn -> return (errData e)
        _ -> throwError e
     Right v -> return v

createInterp safe vars cmds = do
   st <- makeState safe vars icmds
   stref <- newIORef st
   return (Interp stref)
 where icmds = if safe then onlySafe cmds else cmds

mkInterp = mkInterpWithVars []
mkInterpWithVars = createInterp False

interpEvalStr :: BString -> Interp -> IO (Either BString BString)
interpEvalStr s i = runInterpStr (evalTcl ((T.fromBStr s) :: T.TclObj)) i

runInterpStr t i = runInterp t i >>= return . fixErr
  where fixErr (Left x)  = Left (T.asBStr (errData x))
        fixErr (Right v) = Right (T.asBStr v)
