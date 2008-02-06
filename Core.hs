{-# LANGUAGE BangPatterns #-}
module Core (evalTcl, doCond, subst, coreTests) where

import Common
import qualified TclObj as T
import qualified Data.ByteString.Char8 as B
import RToken
import Util
import VarName

import Test.HUnit

evalTcl :: T.TclObj -> TclM RetVal
evalTcl s = runCmds =<< T.asParsed s
{-# INLINE evalTcl #-}

runCmds [x]    = runCmd x
runCmds (x:xs) = runCmd x >> runCmds xs
runCmds []     = ret


subst s = do cmds <- T.asParsed s
             let toks = concatMap uncmd cmds
             evalRToken (CatLst toks)
 where uncmd (Right n,args) = (n:args)
       uncmd (Left (NSRef Local n), args) = ((Lit n):args)
       uncmd (Left n, _) = error (show n)

evalRToken :: RToken -> TclM T.TclObj
evalRToken (Lit s)         = return $ T.mkTclBStr s
evalRToken (CmdTok t)      = runCmd t
evalRToken (VarRef vn)     = varGetNS vn
evalRToken (ArrRef ns n i) = evalRToken i >>= \ni -> varGetNS (NSRef ns (VarName n (Just (T.asBStr ni))))
evalRToken (CatLst l)      = mapM evalRToken l >>= treturn . B.concat . map T.asBStr
evalRToken (Block s p)     = return $ T.fromBlock s p

runCmd :: Cmd -> TclM RetVal
runCmd (n,args) = do
  evArgs <- mapM evalRToken args
  evArgs `seq` go n evArgs
 where go (Left p@(NSRef _ name)) a = callProc name (getProcNS p) a
       go (Right rt) a = do o <- evalRToken rt
                            let name = T.asBStr o
                            callProc name (getProc name) a

callProc pn f args =  do
   mproc <- f
   case mproc of
     Nothing   -> do ukproc <- getProc (pack "unknown")
                     case ukproc of
                       Nothing -> tclErr $ "invalid command name " ++ show pn
                       Just uk -> (procFunction uk) ((T.mkTclBStr pn):args)
     Just proc -> (procFunction proc) args
{-# INLINE callProc #-}

doCond :: T.TclObj -> TclM Bool
doCond str = do
      p <- T.asParsed str
      case p of
        [x]      -> do r <- runCmd x
                       return $! T.asBool r
        _        -> tclErr "Too many statements in conditional"
{-# INLINE doCond #-}

coreTests = TestList [ ]

