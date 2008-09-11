{-# LANGUAGE BangPatterns,OverloadedStrings #-}
module Core (doCond, evalExpr, evalArgs, subst, coreTests) where

import Common
import qualified TclObj as T
import qualified Data.ByteString.Char8 as B
import TclParse (parseSubst, Subst(..), SubstArgs, allSubstArgs)
import TclErr
import Control.Monad.Error
import RToken
import qualified Expr as E
import Util
import VarName (arrName, NSQual(..), parseNSTag, toBStr, parseVarName, VarName(..))

import Test.HUnit

instance Runnable T.TclObj where
  evalTcl s = asParsed s >>= runCmds
  {-# INLINE evalTcl #-}

instance Runnable Cmd where
  evalTcl = runCmd

runCmds :: [Cmd] -> TclM T.TclObj
runCmds cl = case cl of
   [x]    -> runCmd x
   (x:xs) -> runCmd x >> runCmds xs
   []     -> ret
{-# INLINE runCmds #-}


callProc :: NSQual BString -> [T.TclObj] -> TclM T.TclObj
callProc pn args = getCmdNS pn >>= doCall (toBStr pn) args

evalRTokens []     acc = return $! reverse acc
evalRTokens (x:xs) acc = case x of
            Lit s     -> next $ T.fromBStr s 
            LitInt i  -> next $ T.fromInt i
            CmdTok t  -> runCmds t >>= next
            VarRef vn -> varGetNS vn >>= next
            Block s p -> next $ T.fromBlock s p
            ArrRef ns n i -> do
                 ni <- evalArgs [i] >>= return . T.asBStr . head
                 varGetNS (NSQual ns (arrName n ni)) >>= next
            CatLst l -> evalArgs l >>= next . T.fromBStr . B.concat . map T.asBStr
            ExpTok t -> do 
                 [rs] <- evalArgs [t]
                 l <- T.asList rs
                 evalRTokens xs ((reverse l) ++ acc)
   where next !r = evalRTokens xs (r:acc)
         {-# INLINE next #-}

evalArgs :: [RTokCmd] -> TclM [T.TclObj]
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
     Nothing   -> do ukproc <- getUnknownNS >>= maybe (return Nothing) getCmd
                     case ukproc of
                       Nothing -> tclErr $ "invalid command name " ++ show pn
                       Just uk -> uk `applyTo` ((T.fromBStr pn):args)
     Just proc -> proc `applyTo` args 
{-# INLINE doCall #-}

doCond obj = evalExpr obj >>= T.asBool
{-# INLINE doCond #-}

evalExpr e = E.runAsExpr e exprCallback
{-# INLINE evalExpr #-}

exprCallback !v = case v of
    E.VarRef n     -> varGetNS n
    E.FunRef (n,a) -> callProc (NSQual mathfuncTag n) a
    E.TokEval t    -> evalArgs [t] >>= return . head
    E.CmdEval cmdl  -> runCmds cmdl

mathfuncTag = Just (parseNSTag "::tcl::mathfunc")


subst :: SubstArgs -> BString -> TclM BString
subst sargs str = do 
   lst <- elift $ parseSubst sargs str
   getSubsts lst >>= return . B.concat
 where 
    endIfErr f ef = f `catchError` (\e -> if toEnum (errCode e) == ef then return [] else throwError e)
    getSubsts [] = return []
    getSubsts (x:xs) = good `endIfErr` EBreak
      where good = do fx <- f x
                      fxs <- getSubsts xs
                      return (fx:fxs)
    elift x = case x of
               Left e -> tclErr e
               Right (v,_) -> return v
    handleCmdErrs f = f `catchError` handler
      where handler e = case toEnum (errCode e) of
                                EReturn -> return (errData e)
                                EContinue -> return T.empty
                                _         -> throwError e
    f x = case x of
        SStr s -> return s
        SCmd c -> handleCmdErrs (runCmds (subCmdToCmds c)) >>= return . T.asBStr
        SVar v -> do 
           val <- case parseVarName v of 
                    NSQual ns (VarName n (Just ind)) -> 
                         subst allSubstArgs ind >>= \i2 -> varGetNS (NSQual ns (arrName n i2))
                    vn                               -> varGetNS vn
           return (T.asBStr val)

coreTests = TestList []

