module Common where

import qualified Data.ByteString.Char8 as B
import Control.Monad.Error
import qualified TclObj as T
--import Data.IORef
import Control.Concurrent.MVar
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
data TclState = TclState { tclChans :: MVar ChanMap, tclStack :: [TclEnv] }
type TclProc = [T.TclObj] -> TclM RetVal
type ProcMap = Map.Map BString TclProc
type VarMap = Map.Map BString T.TclObj
type ChanMap = Map.Map BString T.TclChan

makeProcMap = Map.fromList . mapFst B.pack
makeState chans envl = do cm <- newMVar chans
                          return (TclState cm envl)

mapSnd f = map (\(a,b) -> (a, f b))
mapFst f = map (\(a,b) -> (f a, b))

getStack = gets tclStack
putStack s = modify (\v -> v { tclStack = s })
modStack :: ([TclEnv] -> [TclEnv]) -> TclM ()
modStack f = getStack >>= \s -> let r = f s in r `seq` putStack r
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

getChan n = do s <- gets tclChans
               m <- (io . readMVar) s
               return (Map.lookup n m)

addChan c = do s <- gets tclChans
               io (modMVar s (Map.insert (T.chanName c) c))

modMVar m f = do v <- takeMVar m
                 putMVar m (f v)

removeChan c = do s <- gets tclChans
                  io (modMVar s (Map.delete (T.chanName c)))

baseChans = Map.fromList (map (\c -> (T.chanName c, c)) T.tclStdChans )

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

uplevel :: Int -> TclM a -> TclM a
uplevel i p = do 
  (curr,new) <- liftM (splitAt i) getStack
  putStack new
  res <- p
  modStack (curr ++)
  return res
{-# INLINE uplevel #-}

upvar n d s = do (e:es) <- getStack
                 putStack ((e { upMap = Map.insert (T.asBStr s) (n, (T.asBStr d)) (upMap e) }):es)
                 ret

ifNothing m e = maybe (tclErr e) return m
{-# INLINE ifNothing #-}

ret :: TclM RetVal
ret = return T.empty

treturn :: BString -> TclM RetVal
treturn = return . T.mkTclBStr 
{-# INLINE treturn #-}

(.==) :: T.TclObj -> String -> Bool
(.==) bs str = (T.asBStr bs) == B.pack str
{-# INLINE (.==) #-}
