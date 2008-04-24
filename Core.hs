{-# LANGUAGE BangPatterns #-}
module Core (evalTcl, doCond, subst, runCmd, callProc, coreTests) where

import Common
import qualified TclObj as T
import qualified Data.ByteString.Char8 as B
import RToken
import Util
import VarName (arrName, NSQual(..))

import Test.HUnit

evalTcl :: T.TclObj -> TclM T.TclObj
evalTcl s = asParsed s >>= runCmds
{-# INLINE evalTcl #-}


runCmds cl = case cl of
   [x]    -> runCmd x
   (x:xs) -> runCmd x >> runCmds xs
   []     -> ret
{-# INLINE runCmds #-}



callProc :: BString -> [T.TclObj] -> TclM T.TclObj
callProc pn args = getCmd pn >>= \pr -> doCall pn pr args

evalRTokens :: [RToken] -> [T.TclObj] -> TclM [T.TclObj] 
evalRTokens []     !acc = return $! reverse acc
evalRTokens (x:xs) !acc = case x of
            Lit s     -> evalRTokens xs ((T.mkTclBStr s):acc)
            LitInt i  -> evalRTokens xs ((T.mkTclInt i):acc)
            Block s p e -> evalRTokens xs ((T.fromBlock s p e):acc)
            CmdTok t  -> nextWith (runCmd t)
            VarRef vn -> nextWith (varGetNS vn)
            ArrRef ns n i -> do
                 ni <- evalRTokens [i] [] >>= return . T.asBStr . head
                 nextWith (varGetNS (NSQual ns (arrName n ni))) 
            CatLst l -> nextWith (evalRTokens l [] >>= treturn . B.concat . map T.asBStr) 
            ExpTok t -> do 
                 [rs] <- evalRTokens [t] [] 
                 l <- T.asList rs
                 evalRTokens xs ((reverse l) ++ acc)
 where nextWith f = f >>= \r -> evalRTokens xs (r:acc)

runCmd :: Cmd -> TclM T.TclObj
runCmd (n,args) = do
  evArgs <- evalRTokens args []
  res <- evArgs `seq` go n evArgs
  return $! res
 where go (Left p@(NSQual _ name)) a = getCmdNS p >>= \pr -> doCall name pr a
       go (Right rt) a = do lst <- evalRTokens [rt] []
                            let (o:rs) = lst ++ a
                            let name = T.asBStr o
                            getCmd name >>= \pr -> doCall name pr rs


doCall !pn !mproc args = do
   case mproc of
     Nothing   -> do ukproc <- getCmd (pack "unknown")
                     case ukproc of
                       Nothing -> tclErr $ "invalid command name " ++ show pn
                       Just uk -> uk `applyTo` ((T.mkTclBStr pn):args)
     Just proc -> proc `applyTo` args 
{-# INLINE doCall #-}

doCond :: T.TclObj -> TclM Bool
doCond obj = do
      p <- asParsed obj
      case p of
        [x]      -> do r <- runCmd x
                       return $! T.asBool r
        _        -> tclErr "Too many statements in conditional"
{-# INLINE doCond #-}

getSubst s = do 
    case asParsed s of
      Just cmds -> do
         let toks = concatMap uncmd cmds
         if all noInterp toks 
           then return (Left (T.asBStr s))
           else return (Right toks)
      Nothing   -> tclErr "subst failed: currently, doesn't work on stuff that we can't tokenize" -- TODO
 where uncmd (Right n,args) = (n:args)
       uncmd (Left (NSQual nst n), args) = if nst == Nothing then ((Lit n):args) else error (show (nst,n))

subst s = getSubst s >>= doeval
  where doeval (Right t) = evalRTokens t [] >>= return . B.concat . map T.asBStr
        doeval (Left s) = return s

coreTests = TestList []

