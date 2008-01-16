module ControlProcs (controlProcs) where

import Common
import Core
import Control.Monad.Error
import qualified TclObj as T
import qualified Data.ByteString.Char8 as B

controlProcs = makeProcMap $ 
  [("while", procWhile), ("if", procIf), ("for", procFor)]

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
       herr (ERet s)  = return s
       herr EContinue = loop `catchError` herr
       herr x         = throwError x
       loop = do condVal <- doCond cond
                 if condVal then evalTcl body >> loop else ret

procWhile _ = argErr "while"

procFor args = case args of
   [start,test,next,body] -> do evalTcl start
                                loop test next body `catchError` herr (loop test next body)
                                
   _                      -> argErr "for"
 where herr _ EBreak  = ret 
       herr _ e       = throwError e
       eatContinue EContinue = ret
       eatContinue v         = throwError v
       loop test next body = do
         c <- doCond test
         if c then (evalTcl body `catchError` eatContinue) >> evalTcl next >> loop test next body
              else ret
