module ControlProcs (controlProcs) where

import Common
import Core
import Control.Monad.Error
import qualified TclObj as T
import qualified Data.ByteString.Char8 as B

controlProcs = makeProcMap $ 
  [("while", procWhile), ("if", procIf), ("for", procFor), 
   ("foreach", procForEach), ("switch", procSwitch)]

procIf (cond:yes:rest) = do
  condVal <- doCond cond
  if condVal then evalTcl yes
    else case rest of
          []      -> ret
          [s,blk] -> if (T.asBStr s) == (B.pack "else") then evalTcl blk else tclErr "Invalid If"
          (s:r)   -> if s .== "elseif" then procIf r else tclErr "Invalid If"
procIf _ = argErr "if"

procWhile [cond,body] = loop `catchError` herr
 where herr EBreak    = ret
       herr EContinue = loop `catchError` herr
       herr x         = throwError x
       loop = do condVal <- doCond cond
                 if condVal then evalTcl body >> loop else ret

procWhile _ = argErr "while"

eatErr f v = (f >> ret) `catchError` \e -> if v == e then ret else throwError e

procFor args = case args of
   [start,test,next,body] -> do evalTcl start
                                loop test next body `eatErr` EBreak
                                ret
   _                      -> argErr "for"
 where loop test next body = do
         c <- doCond test
         if c then (evalTcl body `eatErr` EContinue) >> evalTcl next >> loop test next body
              else ret

procForEach args = 
   case args of
    [vl_,l_,block] -> do 
               vl <- T.asList vl_
               l <- T.asList l_
               let vllen = length vl
               if vllen == 1 
                   then 
                      allowBreak (mapM_ (doSingl (head vl) block) l)
                   else do
                      let chunks = l `chunkBy` vllen
                      allowBreak (mapM_ (doChunk vl block) chunks) 
               ret
    _            -> argErr "foreach"
 where allowBreak f = f `eatErr` EBreak
       doChunk vl block items = do zipWithM_ (\a b -> varSet (T.asBStr a) b) vl (items ++ repeat T.empty) 
                                   evalTcl block `eatErr` EContinue
       doSingl v block i = do varSet (T.asBStr v) i
                              evalTcl block `eatErr` EContinue

chunkBy lst n = let (a,r) = splitAt n lst  
                in a : (if null r then [] else r `chunkBy` n)

procSwitch args = case args of
   [str,pairlst]     -> T.asList pairlst >>= doSwitch str 
   [opt,str,pairlst] -> if opt .== "--" 
                         then T.asList pairlst >>= doSwitch str
                         else argErr "switch"
   _                 -> argErr "switch"

doSwitch str lst = do pl <- toPairs lst
                      switcher str pl False
 where
       switcher _ []      _       = tclErr "impossible condition in \"switch\""
       switcher f [(k,v)] useNext = do
         if f == k || useNext || k .== "default"
             then if v .== "-" then tclErr $ "no body specified for pattern " ++ show k
                               else evalTcl v
             else ret
       switcher f ((k,v):xs) useNext = do
         if f == k || useNext
             then if v .== "-" then switcher f xs True else evalTcl v
             else switcher f xs False

toPairs [a,b]   = return [(a,b)]
toPairs (a:b:r) = liftM ((a,b):) (toPairs r)
toPairs _       = tclErr "list must have even number of elements"
