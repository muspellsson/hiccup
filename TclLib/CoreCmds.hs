module TclLib.CoreCmds (coreCmds) where
import Common
import Control.Monad.Error
import Control.Monad (liftM)
import Util
import Core
import qualified TclObj as T

coreCmds = makeCmdMap $ [
  ("set", procSet),
  ("uplevel", procUpLevel),
  ("return", procReturn),
  ("global", procGlobal),
  ("upvar", procUpVar),
  ("eval", procEval),
  ("catch", procCatch),
  ("unset", procUnset),
  ("rename", procRename),
  ("info", procInfo),
  ("error", procError)]

procSet args = case args of
     [s1,s2] -> varSet (T.asBStr s1) s2
     [s1]    -> varGet (T.asBStr s1)
     _       -> argErr "set"

procUnset args = case args of
     [n]     -> varUnset (T.asBStr n)
     _       -> argErr "unset"

procRename args = case args of
    [old,new] -> renameProc (T.asBStr old) (T.asBStr new) >> ret
    _         -> argErr "rename"

procError [s] = tclErr (T.asStr s)
procError _   = argErr "error"

procEval args = case args of
                 []   -> argErr "eval"
                 [s]  -> evalTcl s
                 _    -> evalTcl (T.objconcat args)

procUpLevel args = case args of
              [p]    -> uplevel 1 (evalTcl p)
              (si:p) -> T.asInt si >>= \i -> uplevel i (procEval p)
              _      -> argErr "uplevel"

procCatch args = case args of
           [s]        -> (evalTcl s >> return T.tclFalse) `catchError` (retInt . retCodeToInt)
           [s,result] -> (evalTcl s >>= varSet (T.asBStr result) >> return T.tclFalse) `catchError` (retReason result)
           _   -> argErr "catch"
 where retCodeToInt c = case c of 
                           (EDie _)  -> 1
                           (ERet _)  -> 2
                           EBreak    -> 3
                           EContinue -> 4
       retReason v e = case e of
                         EDie s -> varSet (T.asBStr v) (T.mkTclStr s) >> return T.tclTrue
                         _      -> retInt . retCodeToInt $ e
       retInt = return . T.mkTclInt

procReturn args = case args of
      [s] -> throwError (ERet s)
      []  -> throwError (ERet T.empty)
      _   -> argErr "return"

procUpVar :: TclCmd
procUpVar args = case args of
     [d,s]    -> doUp 1 d s
     [si,d,s] -> T.asInt si >>= \i -> doUp i d s
     _        -> argErr "upvar"
 where doUp i d s = upvar i (T.asBStr d) (T.asBStr s) >> ret

procGlobal args = case args of
      [] -> argErr "global"
      _  -> mapM_ (inner . T.asBStr) args >> ret
 where inner g = do len <- stackLevel
                    upvar len g g

procInfo = makeEnsemble "info" [
  matchp "locals" localVars,
  matchp "globals" globalVars,
  matchp "vars" currentVars,
  matchp "commands" commandNames,
  noarg "level"    (liftM T.mkTclInt stackLevel),
  ("exists", info_exists),
  ("body", info_body)]
 where noarg n f = (n, no_args n f)
       matchp n f = (n, matchList ("info " ++ n) f)
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
       [n] -> do p <- getProc (T.asBStr n)
                 case p of
                   Nothing -> tclErr $ show (T.asBStr n) ++ " isn't a procedure"
                   Just p  -> treturn (cmdBody p)
       _   -> argErr "info body"

asTclList = return . T.mkTclList . map T.mkTclBStr
