module Core (evalTcl, doCond, coreTests) where

import Common
import qualified TclObj as T
import Control.Monad (liftM)
import qualified Data.ByteString.Char8 as B
import RToken

import Test.HUnit

evalTcl :: T.TclObj -> TclM RetVal
evalTcl s = runCmds =<< T.asParsed s
{-# INLINE evalTcl #-}

runCmds = liftM last . mapM runCmd


evalRToken :: RToken -> TclM T.TclObj
evalRToken (Lit s)      = return $ T.mkTclBStr s
evalRToken (CmdTok t)   = runCmd t
evalRToken (VarRef n)   = varGet2 n Nothing
evalRToken (ArrRef n i) = evalRToken i >>= \ni -> varGet2 n (Just (T.asBStr ni))
evalRToken (CatLst l)   = mapM evalRToken l >>= treturn . B.concat . map T.asBStr
evalRToken (Block s p)  = return $ T.fromBlock s p

runCmd :: Cmd -> TclM RetVal
runCmd (n,args) = do 
  evArgs <- mapM evalRToken args
  proc <- getCmd n
  proc evArgs
 where getCmd (Lit s) = getProc s
       getCmd rt      = evalRToken rt >>= getProc .T.asBStr

doCond :: T.TclObj -> TclM Bool
doCond str = do 
      p <- T.asParsed str
      case p of
        [x]      -> runCmd x >>= return . T.asBool
        _        -> tclErr "Too many statements in conditional"
{-# INLINE doCond #-}

coreTests = TestList [] 
