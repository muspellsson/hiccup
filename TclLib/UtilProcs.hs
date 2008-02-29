{-# LANGUAGE BangPatterns #-}
module TclLib.UtilProcs ( utilProcs ) where

import Data.Time.Clock (diffUTCTime,getCurrentTime,addUTCTime)
import Control.Monad (unless)
import Control.Concurrent (threadDelay)
import Util
import Core (evalTcl, subst)
import Common
import ExprParse
import qualified TclObj as T

utilProcs = makeProcMap [
         ("time", procTime),("source", procSource), 
         ("incr", procIncr), ("expr", procExpr), 
	 ("after", procAfter), ("update", procUpdate)]

procIncr args = case args of
         [vname]     -> incr vname 1
         [vname,val] -> T.asInt val >>= incr vname
         _           -> argErr "incr"

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

procAfter args = 
    case args of 
      [mss]    -> do
            ms <- T.asInt mss
            io $ threadDelay (1000 * ms)
            ret
      [mss,act] -> do
            ms <- T.asInt mss 
	    let secs = (fromIntegral ms) / 1000.0
	    currT <- io getCurrentTime
	    let dline = addUTCTime secs currT
	    evtAdd act dline
      _     -> argErr "after"

procUpdate args = case args of
     [] -> do evts <- evtGetDue
              mapM_ evalTcl evts
              ret
     _  -> argErr "update"

procSource args = case args of
                  [s] -> io (slurpFile (T.asStr s)) >>= evalTcl . T.mkTclBStr
                  _   -> argErr "source"

procExpr args = do  
  al <- mapM subst args 
  let s = concat (map T.asStr al) 
  riExpr s
