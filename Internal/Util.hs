{-# LANGUAGE BangPatterns #-}
module Internal.Util where

import Internal.Types
import qualified TclObj as T
import Control.Monad.Error
import Data.IORef

readRef :: IORef a -> TclM a
readRef !r = (liftIO . readIORef) r
{-# INLINE readRef #-}

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
