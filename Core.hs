{-# LANGUAGE BangPatterns,OverloadedStrings #-}
module Core (doCond, evalExpr, evalArgs, coreTests) where

import Common
import qualified TclObj as T
import qualified Data.ByteString.Char8 as B
import RToken
import qualified Expr as E
import Util
import VarName (arrName, NSQual(..), parseNSTag, toBStr)

import Test.HUnit

instance Runnable T.TclObj where
  evalTcl s = asParsed s >>= runCmds
  {-# INLINE evalTcl #-}

instance Runnable Cmd where
  evalTcl = runCmd


runCmds cl = case cl of
   [x]    -> runCmd x
   (x:xs) -> runCmd x >> runCmds xs
   []     -> ret
{-# INLINE runCmds #-}


callProc :: NSQual BString -> [T.TclObj] -> TclM T.TclObj
callProc pn args = getCmdNS pn >>= doCall (toBStr pn) args

evalRTokens []     acc = return $! reverse acc
evalRTokens (x:xs) acc = case x of
            Lit s     -> nextWith (return $! T.fromBStr s) 
            LitInt i  -> nextWith (return $! T.fromInt i) 
            CmdTok t  -> nextWith (evalTcl t)
            VarRef vn -> nextWith (varGetNS vn)
            Block s p e -> nextWith (return $! T.fromBlock s p e) 
            ArrRef ns n i -> do
                 ni <- evalArgs [i] >>= return . T.asBStr . head
                 nextWith (varGetNS (NSQual ns (arrName n ni))) 
            CatLst l -> nextWith (evalArgs l >>= return . T.fromBStr . B.concat . map T.asBStr) 
            ExpTok t -> do 
                 [rs] <- evalArgs [t]
                 l <- T.asList rs
                 evalRTokens xs ((reverse l) ++ acc)
 where nextWith f = f >>= \(!r) -> evalRTokens xs (r:acc)

evalArgs args = evalRTokens args []
{-# INLINE evalArgs #-}

runCmd :: Cmd -> TclM T.TclObj
runCmd (Cmd n args) = do
  evArgs <- evalArgs args
  res <- go n evArgs
  return $! res
 where go (BasicCmd p@(NSQual _ name)) a = getCmdNS p >>= doCall name a 
       go (DynCmd rt) a = do 
             lst <- evalArgs [rt]
             let (o:rs) = lst ++ a
             let name = T.asBStr o
             getCmd name >>= doCall name rs 


doCall pn args !mproc = do
   case mproc of
     Nothing   -> do ukproc <- getCmd (pack "unknown")
                     case ukproc of
                       Nothing -> tclErr $ "invalid command name " ++ show pn
                       Just uk -> uk `applyTo` ((T.fromBStr pn):args)
     Just proc -> proc `applyTo` args 
{-# INLINE doCall #-}

doCond obj = evalExpr obj >>= return . T.asBool
{-# INLINE doCond #-}

evalExpr e = E.runAsExpr e exprCallback
{-# INLINE evalExpr #-}

exprCallback v = case v of
    E.VarRef n     -> varGetNS n
    E.FunRef (n,a) -> callProc (NSQual mathfuncTag n) a
    E.CmdEval cmd  -> runCmd cmd

mathfuncTag = Just (parseNSTag "::tcl::mathfunc")

coreTests = TestList []

