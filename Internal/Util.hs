{-# LANGUAGE BangPatterns #-}
module Internal.Util where

import Internal.Types
import qualified TclObj as T
import Control.Monad.Error
import Data.IORef
import Control.Monad.State.Strict

readRef :: IORef a -> TclM a
readRef !r = (liftIO . readIORef) r
{-# INLINE readRef #-}

(.=) :: IORef a -> (a -> a) -> TclM ()
(.=) r f = liftIO (modifyIORef r f)
{-# INLINE (.=) #-}

refExtract !ref !f = liftIO (readIORef ref >>= \x -> (return $! f x)) 
{-# INLINE refExtract #-}

whenJust x f = case x of
      Nothing -> return ()
      Just v  -> f $! v
{-# INLINE whenJust #-}

ensure action p = do
   r <- action `catchError` (\e -> p >> throwError e)
   p
   return $! r
{-# INLINE ensure #-}

ret :: TclM T.TclObj
ret = return T.empty
{-# INLINE ret #-}

isJust Nothing = False
isJust _       = True

hasParent nsr = nsr `refExtract` nsParent >>= return . isJust

getFrameVars :: FrameRef -> TclM VarMap
getFrameVars !frref = frref `refExtract` frVars
{-# INLINE getFrameVars #-}

getUpMap :: FrameRef -> TclM UpMap 
getUpMap = (`refExtract` upMap)
{-# INLINE getUpMap #-}

getNSFrame :: NSRef -> TclM FrameRef
getNSFrame !nsref = nsref `refExtract` nsFrame 


getCurrNS = getFrame >>= \fr -> liftIO (readIORef fr >>= \f -> return $! (frNS f))
{-# INLINE getCurrNS #-}

getGlobalNS :: TclM NSRef
getGlobalNS = gets tclGlobalNS
{-# INLINE getGlobalNS #-}

getStack :: TclM TclStack
getStack = gets tclStack
{-# INLINE getStack  #-}

getFrame :: TclM FrameRef
getFrame = do st <- gets tclStack
              case st of
                 (fr:_) -> return $! fr
                 _      -> fail "stack badness"

getCmdCount :: TclM Int
getCmdCount = gets tclCmdCount

io :: IO a -> TclM a
io = liftIO
{-# INLINE io #-}

putStack s = modify (\v -> v { tclStack = s })
{-# INLINE putStack  #-}
modStack :: (TclStack -> TclStack) -> TclM ()
modStack f = get >>= put . (\v -> let !v2 = v { tclStack = f (tclStack v) } in v2)
{-# INLINE modStack #-}

registerWatcher cb = modify (\t -> t { tclCmdWatchers = cb:(tclCmdWatchers t) })

notifyWatchers = do
  gets tclCmdWatchers >>= io . sequence_
