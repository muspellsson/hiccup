module Proc.CodeBlock where
import RToken
import Common
import Data.IORef
import Core
import Types (TclCmdObj(..))
import qualified TclObj as T
import Util


type TclCmd = [T.TclObj] -> TclM T.TclObj

type MRef a = (IORef (Maybe a))
data CompCmd = CompCmd (Maybe (MRef TclCmd)) (Maybe [RToken CompCmd]) Cmd
data CodeBlock = CodeBlock T.TclObj [CompCmd]

toCodeBlock o = do
   cmds <- asParsed o >>= mapM compCmd
   return (CodeBlock o cmds)

compCmd :: Cmd -> TclM CompCmd
compCmd c@(Cmd (BasicCmd _) args) = do
       r <- io $ newIORef Nothing
       nargs <- (mapM compToken args >>= return . Just) `ifFails` Nothing
       return $ CompCmd (Just r) nargs c
compCmd _ = fail "no compile"

compToken tok = case tok of
  CmdTok t -> compCmd t >>= return . CmdTok
  ExpTok t -> compToken t >>= return . ExpTok
  ArrRef mtag n t -> compToken t >>= \nt -> return $ ArrRef mtag n nt
  VarRef v -> return $ VarRef v
  Block s t e -> return (Block s t e)
  Lit s           -> return (Lit s)
  LitInt i        -> return (LitInt i)
  CatLst lst      -> mapM compToken lst >>= return . CatLst

instance Runnable CompCmd where
  evalTcl = evalCompC

evalCompC (CompCmd Nothing _ c) = evalTcl c
evalCompC (CompCmd (Just cref) nargs c@(Cmd (BasicCmd cn) args)) = do
   mcmd <- io $ readIORef cref
   case mcmd of
     Just cmd -> eArgs >>= cmd
     Nothing  -> do
      mcmd2 <- getCmdNS cn
      case mcmd2 of 
        Nothing -> evalTcl c
        Just cmd -> do 
           let act = cmdAction cmd
           io $ writeIORef cref (Just act)
           eArgs >>= act
 where eArgs = case nargs of 
                Nothing -> evalArgs args
                Just al -> evalArgs al
evalCompC (CompCmd (Just _) _ _) = error "SHOULD NEVER HAPPEN"

evalThem [] = ret
evalThem [x] = evalCompC x
evalThem (x:xs) = evalCompC x >> evalThem xs

instance Runnable CodeBlock where
  evalTcl = runCodeBlock

runCodeBlock (CodeBlock _ cl) = evalThem cl
