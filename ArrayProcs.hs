module ArrayProcs (arrayProcs) where
import Common

import qualified TclObj as T
import Control.Monad
import qualified Data.Map as Map

arrayProcs = makeProcMap $
  [("array", procArray)]


procArray :: TclProc
procArray args = case args of
                  [a,n]   -> twoArgs a n
                  [a,b,c] -> threeArgs a b c
                  [n] -> badOpt n
                  _  -> argErr "array"
 where badOpt v = tclErr $ "bad option " ++ show (T.asBStr v)
       twoArgs a1 a2 
         | a1 .== "size" = do arr <- getArray (T.asBStr a2) `ifFails` Map.empty
                              return . T.mkTclInt $ Map.size arr
         | a1 .== "exists" = do b <- (getArray (T.asBStr a2) >> return True) `ifFails` False
                                return (T.fromBool b)
         | otherwise = badOpt a1
       threeArgs a1 a2 a3 
         | a1 .== "set" = do l <- T.asList a3
                             let len = length l
                             if even len 
                                 then mapM_ (\(a,b) -> arrSet (T.asBStr a2) a (T.mkTclBStr b)) (toPairs l) >> ret
                                 else tclErr "list must have even number of elements"
         | otherwise    = badOpt a1

arrSet n i v = varSet2 n (Just i) v
toPairs (a:b:xs) = (a,b) : toPairs xs
toPairs [] = []
toPairs _ = [] 
