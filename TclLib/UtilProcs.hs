{-# LANGUAGE BangPatterns #-}
module TclLib.UtilProcs ( utilProcs ) where

import Util
import Data.Time.Clock (diffUTCTime,getCurrentTime,addUTCTime)
import Control.Monad (unless)
import Control.Concurrent (threadDelay)
import System.Posix.Process (getProcessID)
import Core (evalExpr,subst)
import TclParse (SubstArgs(..), allSubstArgs)
import Common
import Format
import ArgParse
import qualified TclObj as T
import TclLib.LibUtil

utilProcs = makeCmdList [
   ("time", cmdTime),
   ("incr", cmdIncr), 
   ("expr", cmdExpr),
   ("pid", cmdPid),
   ("subst", cmdSubst),
   ("format", cmdFormat),
   ("after", cmdAfter), ("update", cmdUpdate)]

cmdIncr args = case args of
         [vname]     -> incr vname 1
         [vname,val] -> T.asInt val >>= incr vname
         _           -> argErr "incr"

incr :: T.TclObj -> Int -> TclM T.TclObj
incr n !i = do
    let vn = T.asVarName n
    v <- varGetNS vn `ifFails` (T.fromInt 0)
    iv <- T.asInt v
    varSetNS vn $! (T.fromInt (iv + i))
     
cmdPid args = case args of
     [] -> io getProcessID >>= return . T.fromStr . show
     _  -> argErr "pid"

cmdTime args =
   case args of
     [code]     -> do tspan <- dotime code
                      return (T.fromStr (show tspan))
     [code,cnt] -> do count <- T.asInt cnt
                      unless (count > 0) (tclErr "invalid number of iterations in time")
                      ts <- mapM (\_ -> dotime code) [1..count]
                      let str = show ((sum ts) / fromIntegral (length ts))
                      return (T.fromStr (str ++ " per iteration"))
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

cmdExpr args = case args of
  [s] -> evalExpr s 
  []  -> argErr "expr"
  _   -> evalExpr (T.objconcat args) 

cmdFormat args = case args of
   (x:xs) -> formatString (T.asBStr x) xs >>= return . T.fromBStr
   _      -> argErr "format"

substArgs = mkArgSpecs 1 [
     NoArg "nobackslashes" (\s -> s { s_esc = False }),
     NoArg "nocommands" (\s -> s { s_cmds = False }),
     NoArg "novariables" (\s -> s { s_vars = False })
   ]

cmdSubst args_ = do
  (sargs,args) <- parseArgs substArgs allSubstArgs args_
  case args of
   [x] -> subst sargs (T.asBStr x) >>= return . T.fromBStr
   _   -> argErr "subst"
