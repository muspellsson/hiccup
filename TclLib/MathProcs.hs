{-# LANGUAGE BangPatterns #-}
module TclLib.MathProcs (mathProcs, plus, 
        minus,
        times,
        divide,
        equals,
        notEquals,
        lessThan,
        lessThanEq,
        greaterThan,
        greaterThanEq,
	mathTests ) where

import Common
import qualified TclObj as T
import Control.Monad
import System.Random
import Test.HUnit

mathProcs = makeCmdMap $
   [("+", many plus 0), ("*", many times 1), ("-", m2 minus), ("pow", m2 pow), 
    ("sin", onearg sin), ("cos", onearg cos), ("abs", onearg abs),
    ("eq", procEq), ("ne", procNe), ("sqrt", m1 squarert), 
    ("==", procEql), ("!=", procNotEql),
    ("/", m2 divide), ("<", lessThanProc),(">", greaterThanProc),
    mkcmd ">=" greaterThanEq, ("<=",lessThanEqProc), 
    ("rand", procRand), ("srand", procSrand),
    ("!", procNot)]

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

procRand _ = mathRand

mathRand = io randomIO >>= return . T.mkTclDouble

onearg f = m1 inner
 where inner x = do
            d <- T.asDouble x
	    return (T.mkTclDouble (f d))
{-# INLINE onearg #-}

m1 f args = case args of
  [a] -> f a
  _     -> if length args > 1 then tclErr "too many arguments to math function" 
                              else tclErr "too few arguments to math function"
{-# INLINE m1 #-}

many !f !i args = case args of
  [a,b] -> f a b
  _ -> foldM f (T.mkTclInt i) args
{-# INLINE many #-}

m2 f args = case args of
  [a,b] -> f a b
  _     -> if length args > 2 then tclErr "too many arguments to math function" 
                              else tclErr "too few arguments to math function"
{-# INLINE m2 #-}

procNot args = case args of
  [x] -> return $! T.fromBool . not . T.asBool $ x
  _   -> argErr "!"

squarert x = do
    case T.asInt x of
      Just i -> return $! T.mkTclDouble (sqrt (fromIntegral i))
      Nothing -> do
        d1 <- T.asDouble x
	return $! T.mkTclDouble (sqrt d1)

plus x y = do
   case (T.asInt x, T.asInt y) of
       (Just i1, Just i2) -> return $! (T.mkTclInt (i1+i2))
       _ -> do 
           d1 <- T.asDouble x
           d2 <- T.asDouble y
	   return $! T.mkTclDouble (d1+d2)

pow x y = do
   case (T.asInt x, T.asInt y) of
       (Just i1, Just i2) -> return $! (T.mkTclInt (i1^i2))
       _ -> do 
           d1 <- T.asDouble x
           d2 <- T.asDouble y
	   return $! T.mkTclDouble (d1 ** d2)

minus x y = do
   case (T.asInt x, T.asInt y) of
       (Just i1, Just i2) -> return $! (T.mkTclInt (i1-i2))
       _ -> do 
           d1 <- T.asDouble x
           d2 <- T.asDouble y
	   return $! T.mkTclDouble (d1-d2)

times !x !y = do
   case (T.asInt x, T.asInt y) of
       (Just i1, Just i2) -> return $! (T.mkTclInt (i1*i2))
       _ -> do 
           d1 <- T.asDouble x
           d2 <- T.asDouble y
	   return $! T.mkTclDouble (d1*d2)

divide x y = do
   case (T.asInt x, T.asInt y) of
       (Just i1, Just i2) -> return $! (T.mkTclInt (i1 `div` i2))
       _ -> do 
           d1 <- T.asDouble x
           d2 <- T.asDouble y
	   return $! T.mkTclDouble (d1 / d2)

lessThan a b = case tclCompare a b of
                 LT -> T.tclTrue
                 _  -> T.tclFalse

lessThanProc args = case args of
   [a,b] -> return $! lessThan a b
   _     -> argErr "<"

lessThanEq a b = case tclCompare a b of
                   GT -> T.tclFalse
                   _  -> T.tclTrue

lessThanEqProc args = case args of
   [a,b] -> return $! (lessThanEq a b)
   _     -> argErr "<="

greaterThan a b = case tclCompare a b of
                     GT -> T.tclTrue
                     _  -> T.tclFalse

greaterThanProc args = case args of
   [a,b] -> return $! greaterThan a b
   _     -> argErr ">"

greaterThanEq a b = case tclCompare a b of
                     LT -> T.tclFalse
                     _  -> T.tclTrue

equals a b = case tclCompare a b of
               EQ -> T.tclTrue
               _  -> T.tclFalse

procEql args = case args of
   [a,b] -> return $! (equals a b)
   _     -> argErr "=="

notEquals a b = case tclCompare a b of
                 EQ -> T.tclFalse
                 _  -> T.tclTrue

procNotEql args = case args of
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


tclCompare a b =
  case (T.asInt a, T.asInt b) of
     (Just i1, Just i2) -> compare i1 i2
     _  -> case (T.asDouble a, T.asDouble b) of
                  (Just d1, Just d2) -> compare d1 d2
		  _ -> compare (T.asBStr a) (T.asBStr b)
{-# INLINE tclCompare #-}

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

mathTests = TestList [ testProcEq ]

-- # ENDTESTS # --
