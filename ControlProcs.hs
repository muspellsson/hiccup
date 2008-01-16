module ControlProcs (controlProcs) where

import Common
import Core
import Control.Monad.Error
import qualified TclObj as T
import qualified Data.ByteString.Char8 as B

controlProcs = makeProcMap $ 
  [("while", procWhile), ("if", procIf), ("for", procFor), ("foreach", procForEach)]

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

eatErr v e = if v == e then ret else throwError e

procFor args = case args of
   [start,test,next,body] -> do evalTcl start
                                loop test next body `catchError` eatErr EBreak
                                
   _                      -> argErr "for"
 where loop test next body = do
         c <- doCond test
         if c then (evalTcl body `catchError` eatErr EContinue) >> evalTcl next >> loop test next body
              else ret

procForEach args = 
   case args of
    [vl_,l_,block] -> do vl <- T.asList vl_
                         l <- T.asList l_
                         let chunks = l `chunkBy` (length vl)
                         liftM tryLast (mapM (doChunk vl block) chunks) `catchError` (eatErr EBreak)
                         ret
    _            -> argErr "foreach"
 where doChunk vl block items = do zipWithM_ (\a b -> varSet a b) vl (map T.mkTclBStr items ++ repeat (T.empty)) 
                                   evalTcl block `catchError` (eatErr EContinue)
       tryLast [] = T.empty
       tryLast v  = last v

chunkBy lst n = let (a,r) = splitAt n lst  
                in a : (if null r then [] else r `chunkBy` n)
