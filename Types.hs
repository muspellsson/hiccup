{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -XGeneralizedNewtypeDeriving #-}
module Types where

import Control.Monad.Error
import Control.Monad.State.Strict
import qualified Data.Map as Map
import Data.IORef
import qualified TclObj as T
import qualified EventMgr as Evt

import Util
import TclErr
import TclChan

newtype TclM a = TclM { unTclM :: ErrorT Err (StateT TclState IO) a }
 deriving (MonadState TclState, MonadError Err, MonadIO, Monad)

data Namespace = TclNS {
         nsName :: BString,
         nsCmds :: !CmdMap,
         nsFrame :: !FrameRef,
         nsExport :: [BString],
         nsParent :: Maybe NSRef,
         nsChildren :: Map.Map BString NSRef } 


type FrameRef = IORef TclFrame
type NSRef = IORef Namespace

data TclFrame = TclFrame { 
      frVars :: !VarMap, 
      upMap :: !(Map.Map BString (FrameRef,BString)), 
      frNS :: NSRef,
      frTag :: !Int  }

type TclStack = [FrameRef]

data TclState = TclState { 
    tclChans :: ChanMap, 
    tclInterps :: Map.Map BString (IORef TclState),
    tclEvents :: Evt.EventMgr T.TclObj,
    tclStack :: !TclStack, 
    tclGlobalNS :: !NSRef,
    tclCmdCount :: !Int }

type TclCmd = [T.TclObj] -> TclM T.TclObj

type CmdRef = IORef TclCmdObj

data TclCmdObj = TclCmdObj { 
                   cmdName :: BString, 
                   cmdIsProc :: Bool,
                   cmdBody :: BString,  
                   cmdOrigNS :: Maybe NSRef,
                   cmdParent :: Maybe CmdRef,
                   cmdKids :: [CmdRef],
                   cmdAction :: !TclCmd }

type ProcKey = BString
data CmdMap = CmdMap { 
      cmdMapEpoch :: !Int,
      unCmdMap :: !(Map.Map ProcKey CmdRef) 
  } 

type TclArray = Map.Map BString T.TclObj
data TclVar = ScalarVar !T.TclObj | ArrayVar TclArray | Undefined deriving (Eq,Show)
type VarMap = Map.Map BString TclVar

runTclM :: TclM a -> TclState -> IO (Either Err a, TclState)
runTclM code env = runStateT (runErrorT (unTclM code)) env

execTclM c e = do 
  (r,s) <- runTclM c e 
  case r of
    Right _ -> return s
    Left e -> error (show e)
