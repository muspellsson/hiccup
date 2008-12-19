{-# LANGUAGE BangPatterns,OverloadedStrings #-}
module TclLib.ArrayCmds (arrayCmds, arrayInits) where
import Common

import Util
import Match (globMatches, globMatch, exactMatches)
import qualified TclObj as T
import TclObj ((.==))
import Control.Monad
import TclLib.LibUtil
import TclErr
import qualified Data.Map as Map
import VarName 
import qualified Data.ByteString.Char8 as B
import Text.Printf

arrayCmds = makeCmdList [("parray", cmdParray)]
arrayInits = [registerEnsem "array" cmdArray]

cmdParray args = case args of
     [name] -> do let n = T.asBStr name 
                  arr <- getArray n `orElse` (tclErr ((show n) ++ " isn't an array"))
                  let elts = Map.toList arr
                  let max_keylen = maximum . map (B.length . fst) $ elts
                  let maxlen = max_keylen + B.length n + 2
                  mapM_ (showFun (unpack n) maxlen) elts
                  ret
     _      -> argErr "parray"
 where showFun arrname maxlen (a,b) = do
          let namestr = printf "%s(%s)" arrname (unpack a) :: String
          io $ printf "%-*s = %s\n" maxlen namestr (T.asStr b)


cmdArray = mkEnsemble "array" [
              ("get", array_get), ("size", array_size), 
              ("exists", array_exists), ("set", array_set), 
              ("names", array_names), ("unset", array_unset),
              ("statistics", array_statistics)
    ]

arrSet n i v = varSetNS (arrNameNS n i) v

getOrEmpty name = getArray (T.asBStr name) `ifFails` Map.empty

array_statistics args = case args of 
  [_] -> io (putStrLn "no statistics available") >> ret
  _   -> vArgErr "array statistics arrayName"

array_get args = case args of
         [name] -> runGet name (const True)
         [name,pat] ->  runGet name (globMatch (T.asBStr pat))
         _      -> argErr "array get"
 where runGet name filt = do
        arr <- getOrEmpty name
        return . T.fromList $ concatMap (\(k,v) -> [T.fromBStr k, v]) (filter (filt . fst) (Map.toList arr))
 

array_names args = case args of
         [name] -> getKeys name >>= lreturn
         [name,pat] -> getKeys name >>= lreturn . globMatches (T.asBStr pat)
         [name,mode,pat] -> do 
                      keys <- getKeys name
                      let bpat = T.asBStr pat
                      if mode .== "-glob" 
                         then lreturn (globMatches bpat keys)
                         else if mode .== "-exact" 
                                then lreturn (exactMatches bpat keys)
                                else tclErr $ "bad option " ++ show mode
                      
         _      -> argErr "array names"
 where getKeys n = getOrEmpty n >>= return . Map.keys

array_set args = case args of
          [a2,a3] -> do l <- T.asList a3
                        if even (length l)
                           then do let aname = T.asBStr a2
                                   toPairs l >>= mapM_ (\(a,b) -> arrSet aname (T.asBStr a) b)
                                   ret
                           else tclErr "list must have even number of elements"
          _       -> argErr "array set"

array_unset args = case args of
     [name]     -> varUnsetNS (T.asVarName name)
     [name,pat] -> do let n = T.asBStr name 
                      arr <- getArray n
                      let (NSQual nst (VarName vn x)) = parseVarName n
                      when (x /= Nothing) $ tclErr "what the heck just happened"
                      let withInd i = (NSQual nst (VarName vn (Just i)))
                      mapM_ (\ind -> varUnsetNS (withInd ind)) (globMatches (T.asBStr pat) (Map.keys arr))
                      ret
     _          -> vArgErr "array unset arrayName ?pattern?"

array_exists args = case args of
       [a2] -> do b <- (getArray (T.asBStr a2) >> return True) `ifFails` False
                  return (T.fromBool b)
       _    -> argErr "array exists"

array_size args = case args of
       [name] -> do arr <- getOrEmpty name
                    return $ T.fromInt (Map.size arr)
       _    -> argErr "array exists"
