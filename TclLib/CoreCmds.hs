module TclLib.CoreCmds (coreCmds) where
import Common
import Control.Monad.Error
import TclLib.LibUtil
import Control.Monad (liftM)
import Data.Char (isDigit)
import TclErr 
import System (getProgName)
import Match (globMatches)
import Proc.Util (mkProc, mkLambda)
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
    registerProc pname (T.asBStr body) proc
    ret
  _               -> argErr "proc"


cmdSet args = case args of
     [s1,s2] -> varSetNS (T.asVarName s1) s2
     [s1]    -> varGetNS (T.asVarName s1)
     _       -> vArgErr "set varName ?newValue?"

cmdUnset args = case args of
     []       -> argErr "unset"
     vl       -> mapM_ (varUnsetNS . T.asVarName) vl >> ret

cmdRename args = case args of
    [old,new] -> renameCmd (T.asBStr old) (T.asBStr new) >> ret
    _         -> argErr "rename"

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
              _      -> argErr "uplevel"
 where getLevel l = do
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
    _  -> argErr $ st c
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
         unless (T.asStr c == "-code") $ tclErr "invalid flag to return"
         case T.asStr f of
           "ok" -> throwVal EReturn val
           "error" -> throwVal EReturn val -- TODO: This is not right
           "break" -> trampErr EBreak
           "continue" -> trampErr EContinue
           "return"   -> trampErr EReturn
           cv   -> throwError (eDie $ "bad completion val: " ++ show cv)

cmdUpVar args = case args of
     [d,s]    -> doUp 1 d s
     [si,d,s] -> T.asInt si >>= \i -> doUp i d s
     _        -> argErr "upvar"
 where doUp i d s = upvar i (T.asBStr d) (T.asBStr s) >> ret

cmdGlobal args = case args of
      [] -> argErr "global"
      _  -> mapM_ (inner . T.asBStr) args >> ret
 where inner g = do len <- stackLevel
                    upvar len g g

cmdInfo = mkEnsemble "info" [
  matchp "locals" localVars,
  matchp "globals" globalVars,
  matchp "vars" currentVars,
  matchp "commands" (commandNames False),
  matchp "procs" (commandNames True),
  noarg "level"    (liftM T.fromInt stackLevel),
  noarg "cmdcount" (liftM T.fromInt getCmdCount),
  noarg "nameofexecutable" (liftM T.fromStr (io getProgName)),
  ("exists", info_exists),
  noarg "tclversion" (getVar "::tcl_version"),
  ("body", info_body)]
 where noarg n f = (n, no_args n f)
       matchp n f = (n, matchList ("info " ++ n) f)
       getVar = varGetNS . T.asVarName . T.fromStr
       no_args n f args = case args of
                           [] -> f
                           _  -> argErr $ "info " ++ n

matchList name f args = case args of
     []    -> f >>= asTclList
     [pat] -> getMatches pat
     _     -> argErr name
 where getMatches pat = f >>= asTclList . globMatches (T.asBStr pat)

info_exists args = case args of
        [n] -> varExists (T.asBStr n) >>= return . T.fromBool
        _   -> argErr "info exists"

info_body args = case args of
       [n] -> do p <- getCmd (T.asBStr n)
                 case p of
                   Nothing -> tclErr $ show (T.asBStr n) ++ " isn't a procedure"
                   Just p  -> treturn (cmdBody p)
       _   -> argErr "info body"

asTclList = return . T.fromList . map T.fromBStr

cmdApply args = case args of
   (fn:alst) -> mkLambda fn >>= \f -> f alst
   _         -> argErr "apply"
