module TclLib.MathProcs (mathProcs, plus) where
import Common
import qualified TclObj as T
import Control.Monad
import System.Random

mathProcs = makeProcMap $
   [("+", m2 plus), ("*",m (*)), ("-",m (-)), ("pow", m (^)),
    ("/",m div), ("<", t (<)),(">", t (>)),("<=",t (<=)), ("rand", procRand),
    ("srand", procSrand)]
 where m = procMath
       t = procTest

procSrand args = case args of
 [v] -> do i <- T.asInt v
           io (setStdGen (mkStdGen i))
	   ret
 []  -> tclErr "too few arguments to math function"
 _   -> tclErr "too many arguments to math function"

procRand _ = do
     io randomIO >>= return . T.mkTclDouble

m2 f args = case args of
  [a,b] -> f a b
  _     -> tclErr "too many arguments to math function" 

plus x y = do
   case (T.asInt x, T.asInt y) of
       (Just i1, Just i2) -> return $! (T.mkTclInt (i1+i2))
       _ -> do 
           d1 <- T.asDouble x
           d2 <- T.asDouble y
	   return $! T.mkTclDouble (d1+d2)

procMath :: (Int -> Int -> Int) -> TclCmd
procMath op args = case args of
         [s1,s2] -> do res <- liftM2 op (T.asInt s1) (T.asInt s2)
                       return $! (T.mkTclInt res)
         _       -> argErr "math"
{-# INLINE procMath #-}

procTest :: (Int -> Int -> Bool) -> TclCmd
procTest op args = case args of
         [s1,s2] -> do a1 <- T.asInt s1
                       a2 <- T.asInt s2
                       return $! (T.fromBool (op a1 a2))
         _       -> argErr "test"
{-# INLINE procTest #-}

