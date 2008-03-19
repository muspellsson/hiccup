module TclLib.MathProcs (mathProcs, plus, 
              minus,
	      times,
	      divide,
              procEql, procEq, procNe) where
import Common
import qualified TclObj as T
import Control.Monad
import System.Random

mathProcs = makeProcMap $
   [("+", m2 plus), ("*", m2 times), ("-", m2 minus), ("pow", m2 pow),
    ("eq", procEq), ("ne", procNe),
    ("/", m2 divide), ("<", lessThan),(">", greaterThan),("<=",lessThanEq), ("rand", procRand),
    ("srand", procSrand)]

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

m2 f args = case args of
  [a,b] -> f a b
  _     -> if length args > 2 then tclErr "too many arguments to math function" 
                              else tclErr "too few arguments to math function"

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

times x y = do
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

lessThan args = case args of
   [a,b] -> case tclCompare a b of
              LT -> return T.tclTrue
	      _  -> return T.tclFalse
   _     -> argErr "<"

lessThanEq args = case args of
   [a,b] -> case tclCompare a b of
	      GT  -> return T.tclFalse
              _   -> return T.tclTrue
   _     -> argErr "<="

greaterThan args = case args of
   [a,b] -> case tclCompare a b of
              GT -> return T.tclTrue
	      _  -> return T.tclFalse
   _     -> argErr ">"

procEql args = case args of
   [a,b] -> case tclCompare a b of
             EQ -> return T.tclTrue
	     _  -> return T.tclFalse
   _     -> argErr "=="

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
