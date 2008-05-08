{-# LANGUAGE BangPatterns #-}

module TclLib.MathProcs (mathCmds, 
	mathTests ) where

import Common
import qualified TclObj as T
import qualified TObj as T
import Control.Monad
import System.Random
import MathOp
import Test.HUnit

mathCmds = makeCmdList $
   [("+", many plus 0), ("*", many times 1), ("-", m2 minus), ("pow", m2 pow), 
    ("sin", onearg sin), ("cos", onearg cos), ("abs", m1 absfun), ("double", onearg id),
    ("eq", procEq), ("ne", procNe), ("sqrt", m1 squarert), 
    ("==", procEql), ("!=", cmdNotEql), 
    ("/", m2 divide), ("<", lessThanProc),(">", greaterThanProc),
    mkcmd ">=" greaterThanEq, ("<=",lessThanEqProc), 
    ("rand", cmdRand), ("srand", procSrand),
    ("!", cmdNot), ("max", many1 tmax), ("min", many1 tmin)]

mkcmd n f = (n,inner)
 where inner args = case args of
                     [a,b] -> return $! f a b
                     _     -> argErr n

procSrand args = case args of
 [v] -> mathSrand v
 []  -> tclErr "too few arguments to math function"
 _   -> tclErr "too many arguments to math function"

mathSrand v = do
 i <- T.asInt v 
 io (setStdGen (mkStdGen i))
 ret

cmdRand args = case args of
        [] ->  mathRand
        _  ->  tclErr "too many arguments to math function"

mathRand = io randomIO >>= return . T.fromDouble

onearg f = m1 inner
 where inner x = do
            d <- T.asDouble x
	    return (T.fromDouble (f d))
{-# INLINE onearg #-}


m1 f args = case args of
  [a] -> f a
  _     -> if length args > 1 then tclErr "too many arguments to math function" 
                              else tclErr "too few arguments to math function"
{-# INLINE m1 #-}

many1 !f args = case args of
  [a,b]  -> f a b
  (x:xs) -> foldM f x args
  _      -> tclErr "too few arguments to math function"
{-# INLINE many1 #-}


many !f !i args = case args of
  [a,b] -> f a b
  _ -> foldM f (T.fromInt i) args
{-# INLINE many #-}

m2 f args = case args of
  [a,b] -> f a b
  _     -> if length args > 2 then tclErr "too many arguments to math function" 
                              else tclErr "too few arguments to math function"
{-# INLINE m2 #-}

cmdNot args = case args of
  [x] -> opNot x
  _   -> argErr "!"


lessThanProc args = case args of
   [a,b] -> return $! lessThan a b
   _     -> argErr "<"


lessThanEqProc args = case args of
   [a,b] -> return $! (lessThanEq a b)
   _     -> argErr "<="


greaterThanProc args = case args of
   [a,b] -> return $! greaterThan a b
   _     -> argErr ">"


procEql args = case args of
   [a,b] -> return $! (equals a b)
   _     -> argErr "=="


cmdNotEql args = case args of
      [a,b] -> case (T.asInt a, T.asInt b) of
                  (Just ia, Just ib) -> return $! T.fromBool (ia /= ib)
                  _                  -> procNe [a,b]
      _     -> argErr "!="

procEq args = case args of
   [a,b] -> return . T.fromBool $! (T.strEq a b)
   _     -> argErr "eq"

procNe args = case args of
   [a,b] -> return . T.fromBool $! (T.strNe a b)
   _     -> argErr "ne"


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
       int :: Int -> T.TclObj
       int i = T.fromInt i
       str s = T.mkTclStr s

mathTests = TestList [ testProcEq ]

-- # ENDTESTS # --
