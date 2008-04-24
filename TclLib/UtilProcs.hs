{-# LANGUAGE BangPatterns #-}
module TclLib.UtilProcs ( utilProcs ) where

import Data.Time.Clock (diffUTCTime,getCurrentTime,addUTCTime)
import Control.Monad (unless)
import Control.Concurrent (threadDelay)
import Core (evalTcl, subst, callProc)
import Common
import Util (unpack)
import Expr (runAsExpr, runAsBsExpr, CBData(..))
import Data.List (intersperse)
import qualified TclObj as T

utilProcs = makeCmdList [
   ("time", procTime),
   ("incr", procIncr), ("expr", procExpr), 
   ("bsexpr", procBsExpr),
   ("after", cmdAfter), ("update", cmdUpdate)]

procIncr args = case args of
         [vname]     -> incr vname 1
         [vname,val] -> T.asInt val >>= incr vname
         _           -> argErr "incr"

incr :: T.TclObj -> Int -> TclM T.TclObj
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

cmdAfter args = 
    case args of 
      [mss]    -> do
            ms <- T.asInt mss
            io $ threadDelay (1000 * ms)
            ret
      (mss:acts) -> do
            ms <- T.asInt mss 
            let secs = (fromIntegral ms) / 1000.0
            currT <- io getCurrentTime
            let dline = addUTCTime secs currT
            evtAdd (T.objconcat acts) dline
      _     -> argErr "after"

cmdUpdate args = case args of
     [] -> do evts <- evtGetDue
              upglobal (mapM_ evalTcl evts)
              ret
     _  -> argErr "update"
 where upglobal f = do sl <- stackLevel
                       uplevel sl f

procExpr args = do  
  al <- mapM subst args 
  let s = concat $ intersperse " " (map unpack al)
  runAsExpr s exprCallback

exprCallback v = case v of
		VarRef n      -> varGetNS n
		FunRef (n,a) -> callProc n a

procBsExpr args = case args of
  [s] -> runAsBsExpr s exprCallback
  []  -> argErr "bsexpr"
  _   -> runAsBsExpr (T.objconcat args) exprCallback
