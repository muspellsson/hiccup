module Hiccup (runTcl, mkInterp, runInterp, hiccupTests) where

import qualified Data.Map as Map
import System.IO
import Control.Monad.Error
import Data.IORef
import Data.Char (toLower,toUpper)
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified TclObj as T
import Core
import Common

import IOProcs
import ListProcs
import ArrayProcs
import ControlProcs
import Test.HUnit  -- IGNORE


coreProcs, baseProcs, mathProcs :: ProcMap

coreProcs = makeProcMap $
 [("proc",procProc),("set",procSet),("upvar",procUpVar),
  ("uplevel", procUpLevel), ("unset", procUnset),("dump", procDump),("eval", procEval),
  ("return",procReturn),("break",procRetv EBreak),("catch",procCatch),
  ("continue",procRetv EContinue),("eq",procEq),("ne",procNe),
  ("==",procEql), ("error", procError), ("info", procInfo), ("global", procGlobal)]



baseProcs = makeProcMap $
  [("string", procString), ("append", procAppend), 
  ("source", procSource), ("incr", procIncr)]

mathProcs = makeProcMap . mapSnd procMath $
   [("+",(+)), ("*",(*)), ("-",(-)), 
    ("/",div), ("<", toI (<)),(">", toI (>)),("<=",toI (<=)),("!=",toI (/=))]

toI :: (Int -> Int -> Bool) -> (Int -> Int -> Int)
toI n a b = if n a b then 1 else 0

baseEnv = emptyEnv { procs = procMap  }
 where procMap = Map.unions [coreProcs, controlProcs, mathProcs, baseProcs, ioProcs, listProcs, arrayProcs]

data Interpreter = Interpreter (IORef TclState)

mkInterp :: IO Interpreter
mkInterp = do st <- makeState baseChans [baseEnv]
              stref <- newIORef st
              return (Interpreter stref)


runInterp :: BString -> Interpreter -> IO (Either BString BString)
runInterp s = runInterp' (evalTcl (T.mkTclBStr s))

runInterp' t (Interpreter i) = do
                 bEnv <- readIORef i
                 (r,i') <- runTclM t bEnv 
                 writeIORef i i'
                 return (fixErr r)
  where perr (EDie s)    = B.pack s
        perr (ERet v)    = T.asBStr v
        perr EBreak      = B.pack $ "invoked \"break\" outside of a loop"
        perr EContinue   = B.pack $ "invoked \"continue\" outside of a loop"
        fixErr (Left x)  = Left (perr x)
        fixErr (Right v) = Right (T.asBStr v)

runTcl v = mkInterp >>= runInterp v

procSource args = case args of
                  [s] -> io (B.readFile (T.asStr s)) >>= evalTcl . T.mkTclBStr
                  _   -> argErr "source"


procProc, procSet, procReturn, procUpLevel :: TclProc
procSet args = case args of
     [s1,s2] -> varSet (T.asBStr s1) s2 >> return s2
     [s1]    -> varGet (T.asBStr s1)
     _       -> argErr ("set " ++ show args ++ " " ++ show (length args))

procUnset args = case args of
     [n]     -> varUnset (T.asBStr n)
     _       -> argErr "unset"

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

procEql [a,b] = case (T.asInt a, T.asInt b) of
                  (Just ia, Just ib) -> return $! T.fromBool (ia == ib)
                  _                  -> procEq [a,b]
procEql _ = argErr "=="


procEval args = case args of
                 [s] -> evalTcl s
                 _   -> argErr "eval"

procCatch args = case args of
           [s] -> (evalTcl s >> procReturn [T.tclFalse]) `catchError` (return . catchRes)
           _   -> argErr "catch"
 where catchRes (EDie _) = T.tclTrue
       catchRes _        = T.tclFalse

retmod f = \v -> treturn (f `onObj` v)

onObj f o = (f (T.asBStr o))

procString :: TclProc
procString (f:s:args) 
 | f .== "trim" = treturn (T.trim s)
 | f .== "tolower" = retmod (B.map toLower) s
 | f .== "toupper" = retmod (B.map toUpper) s
 | f .== "reverse" = retmod (B.reverse) s
 | f .== "length" = return $ T.mkTclInt (B.length `onObj` s)
 | f .== "index" = case args of 
                          [i] -> do ind <- T.asInt i
                                    if ind >= (B.length `onObj` s) || ind < 0 then ret else treturn $ B.singleton (B.index (T.asBStr s) ind)
                          _   -> tclErr $ "Bad args to string index."
 | otherwise            = tclErr $ "Can't do string action: " ++ show f
procString _ = argErr "string"



procInfo [x]
  | x .== "commands" =  getFrame >>= procList . toObs . Map.keys . procs
  | x .== "level"    =  getStack >>= return . T.mkTclInt . pred . length
  | x .== "vars"     =  do f <- getFrame 
                           procList . toObs $ Map.keys (vars f) ++ Map.keys (upMap f)
  | otherwise        =  tclErr $ "Unknown info command: " ++ show (T.asBStr x)
procInfo [x,y] 
  | x .== "exists"   =  varExists (T.asBStr y) >>= return . T.fromBool
  | otherwise        =  tclErr $ "Unknown info command: " ++ show (T.asBStr x)
procInfo _   = argErr "info"

toObs = map T.mkTclBStr

procAppend args = case args of
            (v:vx) -> do val <- varGet (T.asBStr v) `ifFails` T.empty
                         procSet [v, oconcat (val:vx)]
            _  -> argErr "append"
 where oconcat = T.mkTclBStr . B.concat . map T.asBStr

procIncr :: TclProc
procIncr [vname]     = incr vname 1
procIncr [vname,val] = T.asInt val >>= incr vname
procIncr _           = argErr "incr"

incr :: T.TclObj -> Int -> TclM RetVal
incr n i =  do v <- varGet bsname
               intval <- T.asInt v
               let res = (T.mkTclInt (intval + i))
               varSet bsname res
               return res
 where bsname = T.asBStr n

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
       lastIsArgs = either (== (B.pack "args")) (const False)  . last


parseParams :: BString -> T.TclObj -> TclM ParamList
parseParams name args = T.asList args >>= countRet
 where countRet lst = mapM doArg lst >>= return . mkParamList name
       doArg s = do l <- T.asList s
                    return $ case l of
                              [k,v] -> Right (k,v)
                              _     -> Left s

bindArgs params@(_,hasArgs,pl) args = do
    walkBoth pl args 
  where walkBoth ((Left v):xs)      (a:as) = varSet v a >> walkBoth xs as
        walkBoth ((Left _):_)       []     = badArgs
        walkBoth ((Right (k,_)):xs) (a:as) = varSet k a >> walkBoth xs as
        walkBoth ((Right (k,v)):xs) []     = varSet k (T.mkTclBStr v) >> walkBoth xs []
        walkBoth []                 xl     = if hasArgs then do procList xl >>= varSet (B.pack "args")
                                                        else unless (null xl) badArgs
        badArgs = argErr $ "should be " ++ showParams params

procProc [name,args,body] = do
  let pname = T.asBStr name
  params <- parseParams pname args
  regProc pname (procRunner params body)
procProc x = tclErr $ "proc: Wrong arg count (" ++ show (length x) ++ "): " ++ show (map T.asBStr x)

procRunner pl body args = 
  withScope $ do 
    bindArgs pl args
    evalTcl body `catchError` herr
 where herr (ERet s)  = return s
       herr (EDie s)  = tclErr s
       herr EBreak    = tclErr "invoked \"break\" outside of a loop"
       herr EContinue = tclErr "invoked \"continue\" outside of a loop"


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
