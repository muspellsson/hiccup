module TclLib.ArrayProcs (arrayProcs) where
import Common

import Util
import qualified TclObj as T
import TclObj ((.==))
import Control.Monad
import qualified Data.Map as Map
import VarName 
import Text.Printf

arrayProcs = makeProcMap [("array", procArray), ("parray", procParray)]

procParray args = case args of
     [name] -> do let n = T.asBStr name 
                  arr <- getArray n `orElse` (tclErr ((show n) ++ " isn't an array"))
                  mapM_ (showFun n) (Map.toList arr)
                  ret
     _      -> argErr "parray"
 where showFun n (a,b) = io (printf "%s(%s) = %s\n" (unpack n) (T.asStr a) (T.asStr b))


procArray = makeEnsemble "array" [
              ("get", array_get), ("size", array_size), 
              ("exists", array_exists), ("set", array_set), 
              ("names", array_names), ("unset", array_unset)
    ]

arrSet n i v = varSetHere (arrName n i) v

toPairs (a:b:xs) = (a,b) : toPairs xs
toPairs _ = [] 

getOrEmpty name = getArray (T.asBStr name) `ifFails` Map.empty

array_get args = case args of
         [name] -> runGet name (const True)
         [name,pat] ->  runGet name (globMatch (T.asBStr pat))
         _      -> argErr "array get"
 where runGet name filt = do
        arr <- getOrEmpty name
        return . T.mkTclList $ concatMap (\(k,v) -> [T.mkTclBStr k, v]) (filter (filt . fst) (Map.toList arr))
 

array_names args = case args of
         [name] -> getKeys name >>= retlist
         [name,pat] -> getKeys name >>= retlist . globMatches (T.asBStr pat)
         [name,mode,pat] -> do 
                      keys <- getKeys name
                      let bpat = T.asBStr pat
                      if mode .== "-glob" 
                         then retlist (globMatches bpat keys)
                         else if mode .== "-exact" 
                                then retlist (exactMatches bpat keys)
                                else tclErr $ "bad option " ++ show mode
                      
         _      -> argErr "array names"
 where getKeys n = getOrEmpty n >>= return . Map.keys
       retlist = return . T.mkTclList . map T.mkTclBStr

array_set args = case args of
          [a2,a3] -> do l <- T.asList a3
                        if even (length l)
                           then mapM_ (\(a,b) -> arrSet (T.asBStr a2) (T.asBStr a) b) (toPairs l) >> ret
                           else tclErr "list must have even number of elements"
          _       -> argErr "array set"

array_unset args = case args of
     [name]     -> varUnset (T.asBStr name)
     [name,pat] -> do let n = T.asBStr name 
                      arr <- getArray n
                      let (NSQual nst (VarName vn x)) = parseVarName n
                      let withInd i = (NSQual nst (VarName vn (Just i)))
                      mapM_ (\ind -> varUnsetNS (withInd ind)) (globMatches (T.asBStr pat) (Map.keys arr))
                      ret
     _          -> argErr "array unset"

array_exists args = case args of
       [a2] -> do b <- (getArray (T.asBStr a2) >> return True) `ifFails` False
                  return (T.fromBool b)
       _    -> argErr "array exists"

array_size args = case args of
       [name] -> do arr <- getOrEmpty name
                    return $ T.mkTclInt (Map.size arr)
       _    -> argErr "array exists"
