{-# LANGUAGE BangPatterns #-}
module Core (evalTcl, doCond, subst, coreTests) where

import Common
import qualified TclObj as T
import qualified Data.ByteString.Char8 as B
import RToken
import Util
import VarName (arrName, NSQual(..), isLocal)

import Test.HUnit

evalTcl :: T.TclObj -> TclM RetVal
evalTcl s = runCmds =<< T.asParsed s
{-# INLINE evalTcl #-}

runCmds [x]    = runCmd x
runCmds (x:xs) = runCmd x >> runCmds xs
runCmds []     = ret


getSubst s = do 
    cmds <- T.asParsed s
    let toks = concatMap uncmd cmds
    return (CatLst toks)
 where uncmd (Right n,args) = (n:args)
       uncmd (Left (NSQual nst n), args) = if isLocal nst then ((Lit n):args) else error (show (nst,n))

subst s = getSubst s >>= \t -> evalRTokens [t] [] >>= return . head


evalRTokens :: [RToken] -> [T.TclObj] -> TclM [T.TclObj] 
evalRTokens []     acc = return $! reverse acc
evalRTokens (x:xs) acc = case x of
            Lit s     -> evalRTokens xs ((T.mkTclBStr s):acc)
            LitInt i  -> evalRTokens xs ((T.mkTclInt i):acc)
            CmdTok t  -> nextWith (runCmd t)
            VarRef vn -> nextWith (varGetNS vn)
            Block s p -> evalRTokens xs ((T.fromBlock s p):acc)
            ArrRef ns n i -> do
                 ni <- evalRTokens [i] [] >>= return . T.asBStr . head
                 nextWith (varGetNS (NSQual ns (arrName n ni))) 
            CatLst l -> nextWith (evalRTokens l [] >>= treturn . B.concat . map T.asBStr) 
            ExpTok t -> do 
                 [rs] <- evalRTokens [t] [] 
                 l <- T.asList rs
                 evalRTokens xs ((reverse l) ++ acc)
 where nextWith f = f >>= \r -> evalRTokens xs (r:acc)

runCmd :: Cmd -> TclM RetVal
runCmd (n,args) = do
  evArgs <- evalRTokens args []
  res <- evArgs `seq` go n evArgs
  return $! res
 where go (Left p@(NSQual _ name)) a = getProcNS p >>= \pr -> callProc name pr a
       go (Right rt) a = do lst <- evalRTokens [rt] []
                            let (o:rs) = lst ++ a
                            let name = T.asBStr o
                            getProc name >>= \pr -> callProc name pr rs


callProc !pn !mproc args = do
   case mproc of
     Nothing   -> do ukproc <- getProc (pack "unknown")
                     case ukproc of
                       Nothing -> tclErr $ "invalid command name " ++ show pn
                       Just uk -> (procFn uk) ((T.mkTclBStr pn):args)
     Just proc -> (procFn proc) $! args 
{-# INLINE callProc #-}

doCond :: T.TclObj -> TclM Bool
doCond str = do
      p <- T.asParsed str
      case p of
        [x]      -> do r <- runCmd x
                       return $! T.asBool r
        _        -> tclErr "Too many statements in conditional"
{-# INLINE doCond #-}

coreTests = TestList []

