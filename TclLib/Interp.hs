module TclLib.Interp ( mkInterp, interpEvalStr, interpCmds) where

import Common
import Util
import Control.Monad.Error (throwError)
import Data.IORef
import TclErr
import Internal.InterpSpec
import qualified Data.Map as M
import qualified TclObj as T
import qualified Data.ByteString.Char8 as B
import TclLib.LibUtil
import TclLib (libCmds, libInits)
import Core ()
import ArgParse
import Internal.Types (Interp(..),interpSafe,tclHidden, unCmdMap)
import CmdList 
import Control.Monad (when)
import VarName (nsSep)

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
    ,("hidden", interp_hidden)
    ,("hide", interp_hide)
  ]

toPath i = T.asList i >>= return . map T.asBStr

interp_hidden args = case args of
  []  -> retHidden []
  [p] -> toPath p >>= retHidden
  _  -> argErr "interp hidden path"
 where retHidden path = getInterp path >>= getHidden 

getHidden (Interp i) = io (readIORef i) >>= return . T.fromBList . cmdMapKeys . tclHidden
  where cmdMapKeys = M.keys . unCmdMap

interp_hide args = case args of
  [p,n] -> do 
       let name = T.asBStr n
       checkname name
       toPath p >>= \path -> interpHide path name >> ret
  _     -> argErr "interp hide"
 where checkname n = when (nsSep `B.isInfixOf` n) $ 
                       tclErr "Cannot use namespace qualifiers in hidden command token"

interp_exists :: [T.TclObj] -> TclM T.TclObj
interp_exists args = case args of
    [n] -> do 
     path <- toPath n
     (getInterp path >> return (T.fromBool True)) `orElse` (return $ T.fromBool False)
    _   -> vArgErr "interp exists ?path?"

interp_slaves args = case args of
   [] -> getInterpNames >>= return . T.fromBList
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
  _   -> vArgErr "interp issafe ?path?"
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
    _   -> vArgErr "interp create ?-safe? ?--? ?path?"
 where create safe n = do 
           path <- toPath n
           ir <- io $ mkInterp (emptyInterp { ispecSafe = safe, 
                                              ispecCmds = allCmds,
                                              ispecInits = libInits })
           registerInterp path ir (interpEnsem n ir)
           return n

interpEnsem n ir = mkEnsemble (T.asStr n) [
                     ("eval", interpEval ir)
                     ,("hidden", slave_hidden ir)]

slave_hidden ir args = case args of
  [] -> getHidden ir
  _  -> argErr "hidden"

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

mkInterp spec = do
   st <- makeState (fixCmds spec)
   stref <- newIORef st
   return (Interp stref)
 where fixCmds sp 
        | ispecSafe sp = sp { ispecCmds = onlySafe (ispecCmds sp),
                              ispecHidden = onlyUnsafe (ispecCmds sp)}
        | otherwise    = sp

interpEvalStr :: BString -> Interp -> IO (Either BString BString)
interpEvalStr s i = runInterpStr (evalTcl (T.fromBStr s :: T.TclObj)) i

runInterpStr t i = runInterp t i >>= return . fixErr
  where fixErr (Left x)  = Left (T.asBStr (errData x))
        fixErr (Right v) = Right (T.asBStr v)
