{-# LANGUAGE BangPatterns #-}
module Hiccup (runTcl, runTclWithArgs, mkInterp, runInterp, hiccupTests) where

import qualified Data.Map as Map
import Control.Monad.Error
import Data.IORef
import qualified TclObj as T
import TclObj (strEq,strNe)
import Core
import Common
import Util
import Data.Time.Clock (diffUTCTime,getCurrentTime)

import TclLib.IOProcs
import TclLib.ListProcs
import TclLib.ArrayProcs
import ControlProcs
import TclLib.StringProcs
import NSProcs
import ProcArgs
import Test.HUnit 
import ExprParse

coreProcs = makeProcMap $
 [("proc",procProc),("set",procSet),("upvar",procUpVar),
  ("rename", procRename),
  ("uplevel", procUpLevel), ("unset", procUnset),("eval", procEval),
  ("return",procReturn),("break",procRetv EBreak),("catch",procCatch),
  ("continue",procRetv EContinue),("eq",procEq),("ne",procNe),("!=",procNotEql),
  ("==",procEql), ("error", procError), ("info", procInfo), ("global", procGlobal),
  ("source", procSource), ("incr", procIncr), ("time", procTime), ("expr", procExpr)]

mathProcs = makeProcMap $
   [("+",m (+)), ("*",m (*)), ("-",m (-)), ("pow", m (^)),
    ("/",m div), ("<", t (<)),(">", t (>)),("<=",t (<=))]
 where m = procMath
       t = procTest

baseProcs = Map.unions [coreProcs, controlProcs, nsProcs, mathProcs, ioProcs, listProcs, arrayProcs, stringProcs]

processArgs al = [("argc" * T.mkTclInt (length al)), ("argv" * T.mkTclList al)]
  where (*) name val = (pack name, val)

interpVars = [("tcl_version" * (show hiccupVersion))]
  where (*) name val = (pack name, T.mkTclStr val)

hiccupVersion = 0.4

data Interpreter = Interpreter (IORef TclState)

mkInterp = mkInterpWithArgs []

mkInterpWithArgs :: [BString] -> IO Interpreter
mkInterpWithArgs args = do
              st <- makeState (interpVars ++ (processArgs (map T.mkTclBStr args))) baseProcs
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

runTcl v = mkInterp >>= runInterp v
runTclWithArgs v args = mkInterpWithArgs args >>= runInterp v

procSource args = case args of
                  [s] -> io (slurpFile (T.asStr s)) >>= evalTcl . T.mkTclBStr
                  _   -> argErr "source"


procProc, procSet, procReturn, procUpLevel :: TclProc
procSet args = case args of
     [s1,s2] -> varSet (T.asBStr s1) s2
     [s1]    -> varGet (T.asBStr s1)
     _       -> argErr ("set " ++ show args ++ " " ++ show (length args))

procUnset args = case args of
     [n]     -> varUnset (T.asBStr n)
     _       -> argErr "unset"

procRename args = case args of
    [old,new] -> varRename (T.asBStr old) (T.asBStr new)
    _         -> argErr "rename"

procEq args = case args of
                  [a,b] -> return $! T.fromBool (a `strEq` b)
                  _     -> argErr "eq"

procNe args = case args of
                  [a,b] -> return $! T.fromBool (a `strNe`  b)
                  _     -> argErr "ne"

procMath :: (Int -> Int -> Int) -> TclProc
procMath op args = case args of
         [s1,s2] -> do res <- liftM2 op (T.asInt s1) (T.asInt s2)
                       return $! (T.mkTclInt res)
         _       -> argErr "math"
{-# INLINE procMath #-}

procTest :: (Int -> Int -> Bool) -> TclProc
procTest op args = case args of
         [s1,s2] -> do a1 <- T.asInt s1
                       a2 <- T.asInt s2
                       return $! (T.fromBool (op a1 a2))
         _       -> argErr "test"
{-# INLINE procTest #-}

procNotEql [a,b] = case (T.asInt a, T.asInt b) of
                  (Just ia, Just ib) -> return $! T.fromBool (ia /= ib)
                  _                  -> procNe [a,b]
procNotEql _ = argErr "!="

procEql [a,b] = case (T.asInt a, T.asInt b) of
                  (Just ia, Just ib) -> return $! T.fromBool (ia == ib)
                  _                  -> procEq [a,b]
procEql _ = argErr "=="


procExpr args = do  
  al <- mapM subst args 
  let s = concat (map T.asStr al) 
  riExpr s

procEval args = case args of
                 [s] -> evalTcl s
                 _   -> argErr "eval"

procCatch args = case args of
           [s]        -> (evalTcl s >> return T.tclFalse) `catchError` retIsErr
           [s,result] -> (evalTcl s >>= varSet (T.asBStr result) >> return T.tclFalse) `catchError` (retReason result)
           _   -> argErr "catch"
 where retIsErr (EDie _) = return T.tclTrue
       retIsErr _        = return T.tclFalse
       retReason v e = case e of
                         EDie s -> varSet (T.asBStr v) (T.mkTclStr s) >> return T.tclTrue
                         _      -> retIsErr e

procInfo = makeEnsemble "info" [
  noarg "commands" (commandNames >>= asTclList),
  noarg "locals"   (localVars    >>= asTclList),
  noarg "globals"  (globalVars   >>= asTclList),
  noarg "level"    (liftM T.mkTclInt stackLevel),
  ("vars", info_vars),
  ("exists", info_exists),
  ("body", info_body)]
 where noarg n f = (n, no_args n f)
       no_args n f args = case args of
                           [] -> f
                           _  -> argErr $ "info " ++ n

info_vars args = case args of
     []    -> currentVars >>= asTclList
     [pat] -> currentVars >>= asTclList . globMatch (T.asBStr pat)
     _  -> argErr "info vars"
info_exists args = case args of
        [n] -> varExists (T.asBStr n) >>= return . T.fromBool
        _   -> argErr "info exists"

info_body args = case args of
       [n] -> do p <- getProc (T.asBStr n)
                 case p of
                   Nothing -> tclErr $ show (T.asBStr n) ++ " isn't a procedure"
                   Just p  -> treturn (procBody p)
       _   -> argErr "info body"

asTclList = return . T.mkTclList . map T.mkTclBStr


procIncr [vname]     = incr vname 1
procIncr [vname,val] = T.asInt val >>= incr vname
procIncr _           = argErr "incr"

incr :: T.TclObj -> Int -> TclM RetVal
incr n !i =  varModify (T.asBStr n) $
                 \v -> do ival <- T.asInt v
                          return $! (T.mkTclInt (ival + i))

procTime args =
   case args of
     [code]     -> do tspan <- dotime code
                      return (T.mkTclStr (show tspan))
     [code,cnt] -> do count <- T.asInt cnt
                      unless (count > 0) (tclErr "invalid number of iterations in time")
                      ts <- mapM (\_ -> dotime code) [1..count]
                      let str = show ((sum ts) / fromIntegral (length ts))
                      return (T.mkTclStr (str ++ " per iteration"))
     _      -> argErr "time"
 where dotime code = do
         startt <- io getCurrentTime
         evalTcl code
         endt <- io getCurrentTime
         let tspan = diffUTCTime endt startt
         return tspan


procReturn args = case args of
      [s] -> throwError (ERet s)
      []  -> throwError (ERet T.empty)
      _   -> argErr "return"

procRetv c [] = throwError c
procRetv c _  = argErr $ st c
 where st EContinue = "continue"
       st EBreak    = "break"
       st _         = "??"

procError [s] = tclErr (T.asStr s)
procError _   = argErr "error"

procUpLevel args = case args of
              [p]    -> uplevel 1 (evalTcl p)
              (si:p) -> T.asInt si >>= \i -> uplevel i (procEval p)
              _      -> argErr "uplevel"

procUpVar :: TclProc
procUpVar args = case args of
     [d,s]    -> doUp 1 d s
     [si,d,s] -> T.asInt si >>= \i -> doUp i d s
     _        -> argErr "upvar"
 where doUp i d s = upvar i (T.asBStr d) (T.asBStr s) >> ret

procGlobal args = case args of
      [] -> argErr "global"
      _  -> mapM_ (inner . T.asBStr) args >> ret
 where inner g = do len <- stackLevel
                    upvar len g g >> ret

procProc [name,args,body] = do
  let pname = T.asBStr name
  params <- parseParams pname args
  regProc pname (T.asBStr body) (procRunner params body)
procProc x = tclErr $ "proc: Wrong arg count (" ++ show (length x) ++ "): " ++ show (map T.asBStr x)

procRunner pl body args = do
  locals <- bindArgs pl args
  withLocalScope locals (evalTcl body `catchError` herr)
 where herr (ERet s)  = return s
       herr EBreak    = tclErr "invoked \"break\" outside of a loop"
       herr EContinue = tclErr "invoked \"continue\" outside of a loop"
       herr e         = throwError e


-- # TESTS # --


testProcEq = TestList [
      "1 eq 1 -> t" ~:          (procEq [int 1, int 1]) `is` True
      ,"1 == 1 -> t" ~:         (procEql [int 1, int 1]) `is` True
      ,"' 1 ' == 1 -> t" ~:     procEql [str " 1 ", int 1] `is` True
      ,"' 1 ' eq 1 -> f" ~:     procEq [str " 1 ", int 1] `is` False
      ,"' 1 ' eq ' 1 ' -> t" ~: procEq [str " 1 ", str " 1 "] `is` True
      ,"' 1 ' ne '1' -> t" ~: procNe [str " 1 ", str "1"] `is` True
      ,"'cats' eq 'cats' -> t" ~: procEq [str "cats", str "cats"] `is` True
      ,"'cats' eq 'caps' -> f" ~: procEq [str "cats", str "caps"] `is` False
      ,"'cats' ne 'cats' -> t" ~: procNe [str "cats", str "cats"] `is` False
      ,"'cats' ne 'caps' -> f" ~: procNe [str "cats", str "caps"] `is` True
   ]
 where (?=?) a b = assert (runCheckResult b (Right a))
       is c b = (T.fromBool b) ?=? c
       int i = T.mkTclInt i
       str s = T.mkTclStr s

hiccupTests = TestList [ testProcEq ]

-- # ENDTESTS # --
