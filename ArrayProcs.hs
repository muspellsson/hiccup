module ArrayProcs (arrayProcs) where
import Common

import Util
import qualified TclObj as T
import Control.Monad
import qualified Data.Map as Map
import VarName
import Text.Printf

arrayProcs = makeProcMap $
  [("array", procArray), ("parray", procParray)]

procParray :: TclProc
procParray args = case args of
     [name] -> do let n = T.asBStr name 
                  arr <- getArray n `orElse` (tclErr ((show n) ++ " isn't an array"))
                  mapM_ (showFun n) (Map.toList arr)
                  ret
     _      -> argErr "parray"
 where showFun n (a,b) = io (printf "%s(%s) = %s\n" (unpack n) (T.asStr a) (T.asStr b))


procArray :: TclProc
procArray args = case args of
                  (x:xs) -> case Map.lookup (T.asBStr x) arraySubs of
                              Nothing -> tclErr $ "bad option " ++ show (T.asBStr x) 
                              Just f  -> (procFunction f) xs
                  _  -> argErr "array"

arraySubs = makeProcMap $ [("get", array_Get), ("size", array_Size), ("exists", array_Exists), ("set", array_Set) ]

arrSet n i v = varSet' (VarName n (Just i)) v
toPairs (a:b:xs) = (a,b) : toPairs xs
toPairs [] = []
toPairs _ = [] 


getOrEmpty name = getArray (T.asBStr name) `ifFails` Map.empty

array_Get :: TclProc
array_Get args = case args of
         [name] ->  do arr <- getOrEmpty name
                       return . T.mkTclList $ concatMap (\(k,v) -> [T.mkTclBStr k, v]) (Map.toList arr)
         _      -> argErr "array get"

array_Set args = case args of
          [a2,a3] -> do l <- T.asList a3
                        if even (length l)
                           then mapM_ (\(a,b) -> arrSet (T.asBStr a2) (T.asBStr a) b) (toPairs l) >> ret
                           else tclErr "list must have even number of elements"
          _       -> argErr "array set"

array_Exists args = case args of
       [a2] -> do b <- (getArray (T.asBStr a2) >> return True) `ifFails` False
                  return (T.fromBool b)
       _    -> argErr "array exists"

array_Size args = case args of
       [name] -> do arr <- getOrEmpty name
                    return $ T.mkTclInt (Map.size arr)
       _    -> argErr "array exists"
