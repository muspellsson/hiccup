{-# LANGUAGE BangPatterns,OverloadedStrings #-}
module TclLib.CoreCmds (coreCmds) where
import Common
import Internal.Types (procArgs)
import Control.Monad.Error
import TclLib.LibUtil
import Control.Monad (liftM)
import Data.Char (isDigit)
import TclErr 
import VarName (parseProc, NSQual(..), toBStr)
import System (getProgName)
import Match (globMatches)
import Proc.Util (mkProc, mkLambda)
import Proc.Params
import ArgParse
import Core ()

import qualified Data.ByteString.Char8 as B
import qualified TclObj as T

coreCmds = makeCmdList [
  ("proc", cmdProc),
  ("set", cmdSet),
  ("uplevel", cmdUplevel),
  ("return", cmdReturn),
  ("global", cmdGlobal),
  ("upvar", cmdUpVar),
  ("eval", cmdEval),
  ("catch", cmdCatch),
  ("break", cmdRetv EBreak),
  ("continue", cmdRetv EContinue),
  ("unset", cmdUnset),
  ("rename", cmdRename),
  ("info", cmdInfo),
  ("apply", cmdApply),
  ("error", cmdError)]

cmdProc args = case args of
  [name,alst,body] -> do
    let pname = T.asBStr name
    proc <- mkProc pname alst body
    registerProc pname proc
    ret
  _  -> vArgErr "proc name args body"


cmdSet args = case args of
     [s1,s2] -> varSetNS (T.asVarName s1) s2
     [s1]    -> varGetNS (T.asVarName s1)
     _       -> vArgErr "set varName ?newValue?"

unsetArgs = boolFlagSpec "nocomplain" 0
cmdUnset args_ = do
  let (fl,args) = flagSpan args_
  (nocomp,_) <- parseArgs unsetArgs False fl
  let unsetter = if nocomp then \r -> ((varUnsetNS r) `orElse` ret)
                           else varUnsetNS
  mapM_ (unsetter . T.asVarName) args >> ret

cmdRename args = case args of
    [old,new] -> renameCmd (T.asBStr old) (T.asBStr new) >> ret
    _         -> vArgErr "rename oldName newName"

cmdError [s] = tclErr (T.asStr s)
cmdError _   = argErr "error"

cmdEval args = case args of
                 []   -> argErr "eval"
                 [s]  -> evalTcl s
                 _    -> evalTcl (T.objconcat args)

-- TODO: an error in the uplevel causes the other one to run.. wrong
cmdUplevel args = case args of
              [p]    -> uplevel 1 (evalTcl p)
              (si:p) -> do 
                  let defaulted = getLevel si >>= \i -> uplevel i (cmdEval p)
                  let checkfirst = let str = T.asBStr si in
                                   let d = B.head str
                                   in when (isDigit d || d == '#') (fail $ "expected integer but got " ++ show str)
                  defaulted `orElse` (checkfirst >> uplevel 1 (cmdEval (si:p)))
              _      -> vArgErr "uplevel ?level? command ?arg ...?"

getLevel l = do
         let badlevel = tclErr $ "bad level " ++ show (T.asBStr l)
         case T.asInt l of
            Just i  -> return i
            Nothing -> case B.uncons (T.asBStr l) of 
                         Just ('#', r) -> case B.readInt r of
                                            Just (i,_) -> do
                                                   lev <- stackLevel
                                                   return (lev - i)
                                            _ -> badlevel
                         _ -> badlevel

cmdCatch args = case args of
           [s]        -> (evalTcl s >> retCodeVal e_OK) `catchError` retCode
           [s,result] -> (evalTcl s >>= varSetNS (T.asVarName result) >> retCodeVal e_OK) `catchError` (retReason result)
           _   -> argErr "catch"
 where retReason v e = 
            let setVar = varSetNS (T.asVarName v) (errData e) >> retCode e
            in case toEnum (errCode e) of
                         EError  -> setVar
                         EReturn -> setVar
                         _       -> retCode e
       retCode = retCodeVal . errCode
       retCodeVal = return . T.fromInt

cmdRetv c args = case args of
    [] -> throwError (fromCode c)
    _  -> vArgErr $ st c
 where st EContinue = "continue"
       st EBreak    = "break"
       st _         = "??"

cmdReturn args = case args of
      [s] -> returnVal s
      []  -> returnVal T.empty
      [c,f] -> handleCode c f T.empty
      [c,f,s] -> handleCode c f s
      _   -> argErr "return"
 where throwVal e val = throwError (Err (fromEnum e) (Just val))
       returnVal v = throwError (Err e_RETURN (Just v))
       trampErr x = throwError (ErrTramp (fromCode x))
       handleCode c f val = do
         unless (T.asBStr c == "-code") $ tclErr "invalid flag to return"
         case T.asInt f of 
           -- TODO: Extend this!
           Just 0 -> throwVal EReturn val 
           _ -> case T.asBStr f of
                   "ok" -> throwVal EReturn val
                   "error" -> throwVal EReturn val -- TODO: This is not right
                   "break" -> trampErr EBreak
                   "continue" -> trampErr EContinue
                   "return"   -> trampErr EReturn
                   cv   -> throwError (eDie $ "bad completion val: " ++ show cv)

-- TODO: Allow multiple bindings
cmdUpVar args = case args of
     [d,s]    -> doUp 1 d s
     [si,d,s] -> getLevel si >>= \i -> doUp i d s
     _        -> vArgErr "upvar ?level? otherVar localVar"
 where doUp i d s = upvar i (T.asBStr d) (T.asBStr s) >> ret

cmdGlobal args = case args of
      [] -> vArgErr "global varName ?varName ...?"
      _  -> mapM_ (inner . T.asBStr) args >> ret
 where inner g = do len <- stackLevel
                    upvar len g g

cmdInfo = mkEnsemble "info" [
  matchp "locals" localVars,
  matchp "globals" globalVars,
  matchp "vars" currentVars,
  ("commands", info_commands),
  ("procs", info_procs),
  ("level", info_level),
  noarg "cmdcount" (liftM T.fromInt getCmdCount),
  noarg "nameofexecutable" (liftM T.fromStr (io getProgName)),
  ("exists", info_exists),
  noarg "tclversion" (getVar "::tcl_version"),
  ("body", info_body),
  ("args", info_args)]
 where noarg n f = (n, no_args n f)
       matchp n f = (n, matchList ("info " ++ n) f)
       getVar = varGetNS . T.asVarName . T.fromStr
       no_args n f args = case args of
                           [] -> f
                           _  -> argErr $ "info " ++ n

matchList name f args = case args of
     []    -> f >>= lreturn
     [pat] -> getMatches pat
     _     -> argErr name
 where getMatches pat = f >>= lreturn . globMatches (T.asBStr pat)

-- TODO: Get rid of duplication here
info_procs args = case args of
  []    -> getCmds Nothing True
  [pat] -> getCmds (Just pat) True
  _ -> vArgErr "info procs ?pattern?"

info_commands args = case args of
  []    -> getCmds Nothing False
  [pat] -> getCmds (Just pat) False
  _ -> vArgErr "info commands ?pattern?"

getCmds :: (Maybe T.TclObj) -> Bool -> TclM T.TclObj
getCmds mpat procsOnly = case mpat of
  Nothing  -> commandNames Nothing procsOnly >>= lreturn
  Just pat -> let (NSQual nst p) = parseProc (T.asBStr pat)
                  prefixify n = toBStr (NSQual nst n)
              in commandNames nst procsOnly >>= lreturn . map prefixify . globMatches p

info_level args = case args of
  [] -> liftM T.fromInt stackLevel
  [lev] -> do
       levn <- T.asInt lev
       uplevel levn (getFrameInfo >>= return . T.fromList)
  _   -> vArgErr "info level ?number?"

info_exists args = case args of
        [n] -> varExists (T.asBStr n) >>= return . T.fromBool
        _   -> argErr "info exists"

info_args args = case args of
   [n] -> do
     pr <- getProcInfo (T.asBStr n)
     return $ T.fromBList $ listParams (procArgs pr)
   _   -> vArgErr "info args procname"

info_body args = case args of
    [n] -> getProcInfo (T.asBStr n) >>= treturn . procBody
    _   -> vArgErr "info body procname"


cmdApply args = case args of
   (fn:alst) -> mkLambda fn >>= \f -> f alst
   _         -> argErr "apply"
