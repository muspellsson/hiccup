module TclLib.MathProcs (mathProcs) where
import Common
import qualified TclObj as T
import Control.Monad

mathProcs = makeProcMap $
   [("+",m (+)), ("*",m (*)), ("-",m (-)), ("pow", m (^)),
    ("/",m div), ("<", t (<)),(">", t (>)),("<=",t (<=))]
 where m = procMath
       t = procTest

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

