module Common where

import qualified Data.ByteString.Char8 as B
import Control.Monad.Error
import qualified TclObj as T
import Data.IORef
import Control.Monad.State
import qualified Data.Map as Map

type BString = B.ByteString
type RetVal = T.TclObj -- IGNORE

data Err = ERet !RetVal | EBreak | EContinue | EDie String deriving (Eq)

instance Error Err where
 noMsg    = EDie "An error occurred."
 strMsg s = EDie s

data TclEnv = TclEnv { vars :: VarMap, procs :: ProcMap, upMap :: Map.Map BString (Int,BString) } 
type TclM = ErrorT Err (StateT TclState IO)
data TclState = TclState { tclChans :: IORef ChanMap, tclStack :: [TclEnv] }
type TclProc = [T.TclObj] -> TclM RetVal
type ProcMap = Map.Map BString TclProc
type VarMap = Map.Map BString T.TclObj
type ChanMap = Map.Map BString T.TclChan

getStack = gets tclStack
putStack s = modify (\v -> v { tclStack = s })
modStack :: ([TclEnv] -> [TclEnv]) -> TclM ()
modStack f = getStack >>= putStack . f
getFrame = liftM head getStack

{-# INLINE getStack  #-}
{-# INLINE putStack  #-}

io :: IO a -> TclM a
io = liftIO

tclErr:: String -> TclM a
tclErr = throwError . EDie

argErr s = tclErr ("wrong # of args: " ++ s)

runTclM :: TclM a -> TclState -> IO (Either Err a, TclState)
runTclM code env = runStateT (runErrorT code) env

getChans = do s <- gets tclChans
              (io . readIORef) s

addChan c = do s <- gets tclChans
               io (modifyIORef s (Map.insert (T.chanName c) c))

removeChan c = do s <- gets tclChans
                  io (modifyIORef s (Map.delete (T.chanName c)))

upped s e = Map.lookup s (upMap e)
{-# INLINE upped #-}

varSet :: BString -> T.TclObj -> TclM ()
varSet str v = do when (B.null str) $ tclErr "Empty varname to set!" 
                  (env:es) <- getStack
                  case upped str env of
                    Just (i,s) -> uplevel i (varSet s v)
                    Nothing    -> putStack ((env { vars = Map.insert str v (vars env) }):es)

varGet :: BString -> TclM RetVal
varGet name = do env <- getFrame
                 case upped name env of
                   Nothing    -> Map.lookup name (vars env) `ifNothing` ("can't read \"$" ++ T.asStr name ++ "\": no such variable")
                   Just (i,n) -> uplevel i (varGet n)

uplevel i p = do 
  (curr,new) <- liftM (splitAt i) getStack
  putStack new
  res <- p
  modStack (curr ++)
  return res

upvar n d s = do (e:es) <- getStack
                 putStack ((e { upMap = Map.insert (T.asBStr s) (n, (T.asBStr d)) (upMap e) }):es)
                 ret

{-# INLINE ifNothing #-}
ifNothing m e = maybe (tclErr e) return m

ret :: TclM RetVal
ret = return T.empty
