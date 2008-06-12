{-# OPTIONS_GHC -XGeneralizedNewtypeDeriving #-}
module Proc.CodeBlock where

import Control.Monad.State
import Control.Monad.Error
import Data.Array.IO
import qualified Data.Map as M

import Types (TclCmdObj(..))
import Core
import VarName (NSQual)
import RToken hiding (CmdName)
import Common 
import qualified TclObj as T
import Util


type TclCmd = [T.TclObj] -> TclM T.TclObj

type CmdName = NSQual BString

data CompCmd = CompCmd (Maybe CmdTag) (Maybe [RToken CompCmd]) Cmd

data CodeBlock = CodeBlock CmdCache [CompCmd]
data CmdCache = CmdCache (IOArray Int (Maybe TclCmd, CmdName))

type CmdTag = (Int, CmdName)

type CmdIds = M.Map CmdName Int
data CState = CState Int CmdIds deriving (Show)
type CErr = String

newtype CompM a = CompM { unCompM :: ErrorT CErr (StateT CState IO) a }
 deriving (MonadState CState, MonadError CErr, MonadIO, Monad)

runCompM code s = runStateT (runErrorT (unCompM code)) s

toCodeBlock :: T.TclObj -> TclM CodeBlock
toCodeBlock o = do
   (e_cmds,st) <- asParsed o >>= liftIO . compile
   carr <- makeCmdArray st
   case e_cmds of
     Left e -> tclErr e
     Right cmds -> return (CodeBlock (CmdCache carr) cmds)

compile p = runCompM (mapM compCmd p) (CState 0 M.empty)

makeCmdArray :: CState -> TclM (IOArray Int (Maybe TclCmd, CmdName))
makeCmdArray (CState _ m) = do
   let size = M.size m
   liftIO $ newListArray (0,size-1) (map (\k -> (Nothing,k))(M.keys m))

getTag :: CmdName -> CompM CmdTag
getTag cn = do
  (CState i mi) <- get
  case M.lookup cn mi of
    Just ct -> return (ct,cn)
    Nothing -> do 
        put (CState (succ i) (M.insert cn i mi))
        return (i,cn)


compCmd :: Cmd -> CompM CompCmd
compCmd c@(Cmd (BasicCmd cn) args) = do
       ti <- getTag cn
       nargs <- (mapM compToken args >>= return . Just) `ifFails` Nothing
       return $ CompCmd (Just ti) nargs c
compCmd _ = fail "no compile"

compToken tok = case tok of
  CmdTok t -> compCmd t >>= return . CmdTok
  ExpTok t -> compToken t >>= return . ExpTok
  ArrRef mtag n t -> compToken t >>= \nt -> return $ ArrRef mtag n nt
  VarRef v -> return $ VarRef v
  Block s t e -> return (Block s t e)
  Lit s           -> return $! (Lit s)
  LitInt i        -> return $! (LitInt i)
  CatLst lst      -> mapM compToken lst >>= return . CatLst

invalidateCache (CmdCache carr) = do
   (a,z) <- liftIO $ getBounds carr
   mapM_ (\i -> modInd i (\(_,cn) -> (Nothing,cn))) [a..z]
 where modInd i f = readArray carr i >>= writeArray carr i . f


getCacheCmd (CmdCache carr) (cind,cn) = do
  (mcmd,_) <- liftIO $ readArray carr cind
  case mcmd of
    Just cmd -> return (Just cmd)
    Nothing -> do
       mcmd2 <- getCmdNS cn
       case mcmd2 of
         Nothing -> return Nothing
         Just cmd -> do
           let act = cmdAction cmd
           liftIO $ writeArray carr cind (Just act,cn)
           return (Just act)

evalCompC _ (CompCmd Nothing _ c) = evalTcl c
evalCompC cc (CompCmd (Just ct) nargs c@(Cmd (BasicCmd _) args)) = do
   mcmd <- getCacheCmd cc ct
   case mcmd of
     Just cmd -> eArgs >>= cmd
     Nothing  -> evalTcl c
 where eArgs = case nargs of 
                Nothing -> evalArgs args
                Just al -> evalArgsF (evalCompC cc) al
evalCompC _ (CompCmd (Just _) _ _) = error "SHOULD NEVER HAPPEN"

evalThem _ [] = ret
evalThem cc [x] = evalCompC cc x
evalThem cc (x:xs) = evalCompC cc x >> evalThem cc xs

instance Runnable CodeBlock where
  evalTcl = runCodeBlock

runCodeBlock (CodeBlock cc cl) = evalThem cc cl
