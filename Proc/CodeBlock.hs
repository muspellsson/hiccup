{-# OPTIONS_GHC -XGeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns,OverloadedStrings #-}
module Proc.CodeBlock (toCodeBlock, runCodeBlock, CodeBlock) where

import Control.Monad.State
import Control.Monad.Error
import Data.Array.IO
import qualified Data.Map as M
import VarName (arrName, NSQual(..), VarName, NSTag)

import qualified Data.ByteString.Char8 as B
import Core
import qualified RToken as R
import RToken (asParsed,Cmd(..), RTokCmd)
import Common 
import qualified TclObj as T
import Util


type CmdName = NSQual BString
data CmdTag = CmdTag !Int !CmdName

data CompCmd = CompCmd (Maybe CmdTag) (Either [R.RTokCmd] [CToken]) Cmd

data CodeBlock = CodeBlock CmdCache [CompCmd]

type TclCmd = [T.TclObj] -> TclM T.TclObj
type CacheEntry = (Maybe TclCmd, CmdName)
data CmdCache = CmdCache !(IOArray Int CacheEntry)


type CmdIds = M.Map CmdName Int
data CState = CState Int CmdIds deriving (Show)
type CErr = String

newtype CompM a = CompM { unCompM :: ErrorT CErr (StateT CState IO) a }
 deriving (MonadState CState, MonadError CErr, MonadIO, Monad)

runCompM code s = runStateT (runErrorT (unCompM code)) s

toCodeBlock :: T.TclObj -> TclM CodeBlock
toCodeBlock o = do
   (e_cmds,st) <- R.asParsed o >>= liftIO . compile
   carr <- makeCmdArray st >>= return . CmdCache
   registerWatcher (invalidateCache carr)
   case e_cmds of
     Left e -> tclErr e
     Right cmds -> return (CodeBlock carr cmds)

compile p = runCompM (mapM compCmd p) (CState 0 M.empty)

makeCmdArray :: CState -> TclM (IOArray Int (Maybe TclCmd, CmdName))
makeCmdArray (CState _ m) = do
   let size = M.size m
   liftIO $ newListArray (0,size-1) (map (\k -> (Nothing,k))(M.keys m))

getTag :: CmdName -> CompM CmdTag
getTag cn = do
  (CState i mi) <- get
  case M.lookup cn mi of
    Just ct -> return (CmdTag ct cn)
    Nothing -> do 
        put (CState (succ i) (M.insert cn i mi))
        return (CmdTag i cn)


compCmd :: Cmd -> CompM CompCmd
compCmd c@(Cmd (R.BasicCmd cn) args) = do
       ti <- getTag cn
       nargs <- (mapM compToken args >>= return . Right) `ifFails` (Left args)
       return $ CompCmd (Just ti) nargs c
compCmd _ = fail "no compile"

data CToken = Lit !T.TclObj | CatLst [CToken] 
              | CmdTok [CompCmd]| ExpTok (CToken)
              | VarRef !(NSQual VarName) | ArrRef !(Maybe NSTag) !BString (CToken)
              | Block !T.TclObj

compToken tok = case tok of
  R.CmdTok t -> mapM compCmd t >>= return . CmdTok
  R.ExpTok t -> compToken t >>= return . ExpTok
  R.ArrRef mtag n t -> compToken t >>= \nt -> return $ ArrRef mtag n nt
  R.VarRef v -> return $ VarRef v
  R.Block s p -> return (Block (T.fromBlock s p))
  R.Lit s           -> return $! Lit (T.fromBStr s)
  R.LitInt i        -> return $! Lit (T.fromInt i)
  R.CatLst lst      -> mapM compToken lst >>= return . CatLst

invalidateCache (CmdCache carr) = do
   (a,z) <- liftIO $ getBounds carr
   mapM_ (\i -> modInd i (\(_,cn) -> (Nothing,cn))) [a..z]
 where modInd i f = readArray carr i >>= writeArray carr i . f


getCacheCmd (CmdCache carr) (CmdTag cind cn) = do
  (mcmd,_) <- liftIO $! readArray carr cind
  case mcmd of
    Just _ -> return mcmd
    Nothing -> do
       mcmd2 <- getCmdNS cn
       case mcmd2 of
         Nothing -> return Nothing
         Just cmd -> do
           let act al = cmd `applyTo` al
           let jact = Just act
           liftIO $ writeArray carr cind (jact,cn)
           return jact

evalCompC cc (CompCmd mct nargs c) = 
  case mct of
    Nothing -> evalTcl c
    Just ct -> do
      mcmd <- getCacheCmd cc ct
      case mcmd of
        Just cmd -> eArgs >>= cmd
        Nothing  -> evalTcl c
 where eArgs = case nargs of 
                Left args -> evalArgs args
                Right al  -> evalCompArgs (evalThem cc) al

evalCompArgs cmdFn al = evalCTokens al [] where
    evalCTokens []     acc = return $! reverse acc
    evalCTokens (x:xs) acc = case x of
            Lit s     -> nextWith (return $! s) 
            CmdTok t  -> nextWith (cmdFn t)
            Block o   -> nextWith (return $! o)
            VarRef vn -> nextWith (varGetNS vn)
            ArrRef ns n i -> do
                 ni <- evalArgs_ [i] >>= return . T.asBStr . head
                 nextWith (varGetNS (NSQual ns (arrName n ni))) 
            CatLst l -> nextWith (evalArgs_ l >>= return . T.fromBStr . B.concat . map T.asBStr) 
            ExpTok t -> do 
                 [rs] <- evalArgs_ [t]
                 l <- T.asList rs
                 evalCTokens xs ((reverse l) ++ acc)
     where nextWith f = f >>= \(!r) -> evalCTokens xs (r:acc)
           {-# INLINE nextWith #-}
           evalArgs_ args = {-# SCC "evalArgs_" #-} evalCTokens args []

evalThem cc lst = go lst
 where run_eval = evalCompC cc 
       go []     = ret
       go [x]    = run_eval x
       go (x:xs) = run_eval x >> go xs

instance Runnable CodeBlock where
  evalTcl = runCodeBlock

runCodeBlock (CodeBlock cc cl) = evalThem cc cl
