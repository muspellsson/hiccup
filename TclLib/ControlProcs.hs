{-# LANGUAGE BangPatterns #-}
module TclLib.ControlProcs (controlCmds) where

import Common
import Core
import Match (MatchType(..), matchFun)
import Control.Monad.Error
import qualified TclObj as T
import TclErr
import TclObj ((.==))
import TclLib.LibUtil
import Util
import ArgParse

controlCmds = makeCmdList $
  [("while", cmdWhile), ("if", cmdIf), 
   ("for", cmdFor), ("foreach", cmdForEach), 
   ("switch", cmdSwitch)]

cmdIf (cond:yes:rest) = do
  condVal <- doCond cond
  if condVal then evalTcl yes
    else case rest of
          []      -> ret
          [s,blk] -> if (T.asBStr s) == (pack "else") then evalTcl blk else tclErr "Invalid If"
          (s:r)   -> if s .== "elseif" then cmdIf r else tclErr "Invalid If"
cmdIf _ = argErr "if"


cmdWhile args = case args of
  [cond,body] -> whileLoop cond body
  _           -> argErr "while"

whileLoop cond body = loop `catchError` herr
 where herr e = case toEnum (errCode e) of
           EBreak    -> ret
           EContinue -> loop `catchError` herr
           _         -> throwError e
       loop = do
         condVal <- doCond cond
         if condVal then evalTcl body >> loop else ret

eatErr f v = (f >> ret) `catchError` \e -> if v == (toEnum (errCode e)) then ret else throwError e
{-# INLINE eatErr #-}

cmdFor args = case args of
   [start,test,next,body] -> do evalTcl start
                                loop test next body `eatErr` EBreak
                                ret
   _                      -> argErr "for"
 where loop test next body = do
         c <- doCond test
         if c then (evalTcl body `eatErr` EContinue) >> evalTcl next >> loop test next body
              else ret

cmdForEach args =
   case args of
    [vl_,l_,block] -> do
               vl <- T.asList vl_
               l <- T.asList l_
               let vllen = length vl
               if vllen == 1
                   then allowBreak (mapM_ (doSingl (head vl) block) l)
                   else do
                      let chunks = l `chunkBy` vllen
                      allowBreak (mapM_ (doChunk vl block) chunks)
               ret
    _            -> argErr "foreach"
 where allowBreak f = f `eatErr` EBreak
       doChunk vl block items = do zipWithM_ (\a b -> varSetNS (T.asVarName a) b) vl (items ++ repeat T.empty)
                                   evalTcl block `eatErr` EContinue
       doSingl v block i = do varSetNS (T.asVarName v) i
                              evalTcl block `eatErr` EContinue

chunkBy lst n = let (a,r) = splitAt n lst
                in a : (if null r then [] else r `chunkBy` n)

switchArgs = mkArgSpecs 0 [
                NoArg "exact" ((ExactMatch):), 
                NoArg "glob" ((GlobMatch):)] 

cmdSwitch args_ = do
  let (fl,args) = flagSpan args_
  (tl,_) <- parseArgs switchArgs [] fl
  mt <- mtype tl
  case args of
   [str,pairlst]     -> T.asList pairlst >>= doSwitch (matcher mt str) 
   _                 -> argErr "switch"
 where matcher m s = let bs = T.asBStr s in \o -> matchFun m (T.asBStr o) bs
       showMatch ExactMatch = "-exact"
       showMatch GlobMatch = "-glob"
       mtype ml = case ml of
         []  -> return ExactMatch
         [x] -> return x
         (x:y:_) -> tclErr $ "bad option " ++ showMatch y ++ ": " ++ showMatch x ++ " option already found"

doSwitch matchP lst = do 
   pl <- toPairs lst
   switcher pl False
 where switcher [(k,v)] useNext = do
         if matchP k || useNext || k .== "default"
             then if v .== "-" then tclErr $ "no body specified for pattern " ++ show k
                               else evalTcl v
             else ret
       switcher ((k,v):xs) useNext = do
         if matchP k || useNext
             then if v .== "-" then switcher xs True else evalTcl v
             else switcher xs False
       switcher []      _       = tclErr "impossible condition in \"switch\""

toPairs [a,b]   = return [(a,b)]
toPairs (a:b:r) = liftM ((a,b):) (toPairs r)
toPairs _       = tclErr "list must have even number of elements"
