module Hiccup (runTcl, runTclWithArgs, mkInterp, runInterp, hiccupTests) where

import qualified Data.Map as Map
import System.IO
import Control.Monad.Error
import Data.IORef
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified TclObj as T
import Core
import Common
import Util
import Data.Time.Clock (diffUTCTime,getCurrentTime) 

import IOProcs
import ListProcs
import ArrayProcs
import ControlProcs
import StringProcs
import Test.HUnit  -- IGNORE


coreProcs, mathProcs :: ProcMap

coreProcs = makeProcMap $
 [("proc",procProc),("set",procSet),("upvar",procUpVar),
  ("rename", procRename),
  ("uplevel", procUpLevel), ("unset", procUnset),("dump", procDump),("eval", procEval),
  ("return",procReturn),("break",procRetv EBreak),("catch",procCatch),
  ("continue",procRetv EContinue),("eq",procEq),("ne",procNe),("!=",procNotEql),
  ("==",procEql), ("error", procError), ("info", procInfo), ("global", procGlobal),
  ("source", procSource), ("incr", procIncr), ("time", procTime)]

mathProcs = makeProcMap . mapSnd procMath $
   [("+",(+)), ("*",(*)), ("-",(-)), ("pow", (^)),
    ("/",div), ("<", toI (<)),(">", toI (>)),("<=",toI (<=))]

toI :: (Int -> Int -> Bool) -> (Int -> Int -> Int)
toI n a b = if n a b then 1 else 0

baseEnv = emptyEnv 
baseProcs = Map.unions [coreProcs, controlProcs, 
                   mathProcs, ioProcs, listProcs, arrayProcs, stringProcs]

envWithArgs al = baseEnv { vars = Map.fromList [("argc" * T.mkTclInt (length al)), ("argv" * T.mkTclList al)] } 
  where (*) name val = (pack name, ScalarVar val)

data Interpreter = Interpreter (IORef TclState)

mkInterp :: IO Interpreter
mkInterp = do st <- makeState baseChans [baseEnv] baseProcs
              stref <- newIORef st
              return (Interpreter stref)

mkInterpWithArgs :: [BString] -> IO Interpreter
mkInterpWithArgs args = do 
              st <- makeState baseChans [envWithArgs (map T.mkTclBStr args)] baseProcs
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
                  [s] -> io (B.readFile (T.asStr s)) >>= evalTcl . T.mkTclBStr
                  _   -> argErr "source"


procProc, procSet, procReturn, procUpLevel :: TclProc
procSet args = case args of
     [s1,s2] -> varSet (T.asBStr s1) s2
     [s1]    -> varGetRaw (T.asBStr s1)
     _       -> argErr ("set " ++ show args ++ " " ++ show (length args))

procUnset args = case args of
     [n]     -> varUnset (T.asBStr n)
     _       -> argErr "unset"

procRename args = case args of
    [old,new] -> varRename (T.asBStr old) (T.asBStr new)
    _         -> argErr "rename"

procDump _ = varDump >> ret

procEq args = case args of
                  [a,b] -> return $ T.fromBool (a == b)
                  _     -> argErr "eq"

procNe args = case args of
                  [a,b] -> return $ T.fromBool (a /= b)
                  _     -> argErr "ne"

procMath :: (Int -> Int -> Int) -> TclProc
procMath op [s1,s2] = liftM2 op (T.asInt s1) (T.asInt s2) >>= return . T.mkTclInt
procMath _ _       = argErr "math"
{-# INLINE procMath #-}

procNotEql [a,b] = case (T.asInt a, T.asInt b) of
                  (Just ia, Just ib) -> return $! T.fromBool (ia /= ib)
                  _                  -> procNe [a,b]
procNotEql _ = argErr "!="

procEql [a,b] = case (T.asInt a, T.asInt b) of
                  (Just ia, Just ib) -> return $! T.fromBool (ia == ib)
                  _                  -> procEq [a,b]
procEql _ = argErr "=="


procEval args = case args of
                 [s] -> evalTcl s
                 _   -> argErr "eval"

procCatch args = case args of
           [s]        -> (evalTcl s >> procReturn [T.tclFalse]) `catchError` (return . catchRes)
           [s,result] -> (evalTcl s >>= varSet (T.asBStr result) >> procReturn [T.tclFalse]) `catchError` (\e -> retReason result e >> return (catchRes e))
           _   -> argErr "catch"
 where catchRes (EDie _) = T.tclTrue
       catchRes _        = T.tclFalse
       retReason v e = case e of
                         EDie s -> varSet (T.asBStr v) (T.mkTclStr s) >> return ()
                         _      -> return ()
                         

procInfo (x:xs)
  | x .== "commands" =  info_commands
  | x .== "level"    =  info_level
  | x .== "vars"     =  info_vars
  | x .== "locals"   =  info_locals
  | x .== "globals"  =  info_globals xs
  | x .== "exists"   =  info_exists xs
  | x .== "body"     =  info_body xs
  | otherwise        =  tclErr $ "Unknown info command: " ++ show (T.asBStr x)
procInfo _   = argErr "info"

info_commands = getProcMap >>= procList . toObs . Map.keys
info_level = getStack >>= return . T.mkTclInt . pred . length
info_vars = do f <- getFrame
               procList . toObs $ Map.keys (vars f) ++ Map.keys (upMap f)
info_locals = getFrame >>= procList . toObs . Map.keys . vars
info_exists args = case args of
        [n] -> varExists (T.asBStr n) >>= return . T.fromBool
        _   -> argErr "info exists"

info_globals args = case args of
          [] -> getStack >>= procList . toObs . Map.keys . vars . last
          _  -> argErr "info globals"
info_body args = case args of
       [n] -> do p <- getProcRaw (T.asBStr n)
                 case p of 
                   Nothing -> tclErr $ show (T.asBStr n) ++ " isn't a procedure"
                   Just p  -> treturn (procBody p)
       _   -> argErr "info body"

toObs = map T.mkTclBStr


procIncr :: TclProc
procIncr [vname]     = incr vname 1
procIncr [vname,val] = T.asInt val >>= incr vname
procIncr _           = argErr "incr"

incr :: T.TclObj -> Int -> TclM RetVal
incr n i =  varModify (T.asBStr n) $ 
                 \v -> do ival <- T.asInt v 
                          return (T.mkTclInt (ival + i))


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

procUpVar args = case args of
     [d,s]    -> upvar 1 d s
     [si,d,s] -> T.asInt si >>= \i -> upvar i d s
     _        -> argErr "upvar"

procGlobal args@(_:_) = mapM_ inner args >> ret
 where inner g = do lst <- getStack
                    let len = length lst - 1
                    upvar len g g
procGlobal _         = argErr "global"


type ArgList = [Either BString (BString,BString)]

showParams (n,hasArgs,pl) = show ((n:(map arg2name pl)) `joinWith` ' ') ++ if hasArgs then " ..." else ""

arg2name arg = case arg of
               Left s      -> s
               Right (k,_) -> B.cons '?' (B.snoc k '?')

type ParamList = (BString, Bool, ArgList)

mkParamList :: BString -> ArgList -> ParamList
mkParamList name lst = (name, hasArgs, used)
 where hasArgs = (not . null) lst && (lastIsArgs lst)
       used = if hasArgs then init lst else lst
       lastIsArgs = either (== (pack "args")) (const False)  . last


parseParams :: BString -> T.TclObj -> TclM ParamList
parseParams name args = T.asList (T.asBStr args) >>= countRet
 where countRet lst = mapM doArg lst >>= return . mkParamList name
       doArg s = do l <- T.asList s
                    return $ case l of
                              [k,v] -> Right (k,v)
                              _     -> Left s

bindArgs params@(_,hasArgs,pl) args = do
    walkBoth pl args 
  where walkBoth ((Left v):xs)      (a:as) = varSetLocalVal v a >> walkBoth xs as
        walkBoth ((Left _):_)       []     = badArgs
        walkBoth ((Right (k,_)):xs) (a:as) = varSetLocalVal k a >> walkBoth xs as
        walkBoth ((Right (k,v)):xs) []     = varSetLocalVal k (T.mkTclBStr v) >> walkBoth xs []
        walkBoth []                 xl     = if hasArgs then do procList xl >>= varSetLocalVal (pack "args")
                                                        else if (null xl) then ret else badArgs
        badArgs = argErr $ "should be " ++ showParams params

procProc [name,args,body] = do
  let pname = T.asBStr name
  params <- parseParams pname args
  regProc pname (T.asBStr body) (procRunner params body)
procProc x = tclErr $ "proc: Wrong arg count (" ++ show (length x) ++ "): " ++ show (map T.asBStr x)

procRunner pl body args = 
  withScope $ do 
    bindArgs pl args
    evalTcl body `catchError` herr
 where herr (ERet s)  = return s
       herr EBreak    = tclErr "invoked \"break\" outside of a loop"
       herr EContinue = tclErr "invoked \"continue\" outside of a loop"
       herr e         = throwError e


-- # TESTS # --


run = runWithEnv [baseEnv]

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
 where (?=?) a b = assert (run b (Right a))
       is c b = (T.fromBool b) ?=? c
       int i = T.mkTclInt i
       str s = T.mkTclStr s

hiccupTests = TestList [ testProcEq ]

-- # ENDTESTS # --
