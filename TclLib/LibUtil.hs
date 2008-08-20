{-# LANGUAGE BangPatterns,OverloadedStrings #-}
module TclLib.LibUtil (
    treturn
    ,lreturn
    ,vArgErr
    ,toPairs
    ,toIndex
    ,mkEnsemble
    ,makeCmdList
    ,nsCmdList
    ,mergeCmdLists
    ,makeNsCmdList
    ,safeCmds
    ,unsafeCmds
  ) where

import Common
import Util
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as B
import qualified TclObj as T

import CmdList

vArgErr s = argErr ("should be " ++ show s)

treturn :: BString -> TclM T.TclObj
treturn = return . T.fromBStr
{-# INLINE treturn #-}

lreturn :: [BString] -> TclM T.TclObj
lreturn = return . T.fromBList

toPairs [a,b]   = return [(a,b)]
toPairs (a:b:r) = toPairs r >>= return . ((a,b):)
toPairs _       = tclErr "list must have even number of elements"

toIndex :: (Monad m) => Int -> T.TclObj -> m Int
toIndex len i = case T.asInt i of
                Nothing -> tryEnd
                Just iv -> return iv
 where ibs = T.asBStr i 
       lastInd = len - 1
       badIndex = fail "bad index"
       tryEnd = if ibs `B.isPrefixOf` "end" && not (B.null ibs)
                  then return lastInd
                  else do let (ip,is) = B.splitAt (B.length "end-") ibs
                          if ip == "end-"
                              then case B.readInt is of
                                       Just (iv,_) -> return (lastInd - iv)
                                       _           -> badIndex
                              else badIndex

mkEnsemble name subs = top
  where top args = 
           case args of
             (x:xs) -> case plookup (T.asBStr x) of
                         Just f  -> f xs
                         Nothing -> eunknown (T.asBStr x) xs
             []  -> argErr $ " should be \"" ++ name ++ " subcommand ?arg ...?\""
        eunknown n al = 
            let names = cmdMapNames subMap
            in case completes n names of
                 [x] -> case (B.length n > 1, plookup x) of
                          (True, Just p) -> p al
                          _              -> no_match n names
                 _   -> no_match n names
        no_match n lst = tclErr $ "unknown or ambiguous subcommand " ++ show n ++ ": must be " 
			                        ++ commaList "or" (map unpack lst)
        subMap = Map.fromList (mapFst pack subs)
        plookup s = Map.lookup s subMap
        cmdMapNames = Map.keys
        completes s lst = case filter (s `B.isPrefixOf`) lst of 
                            [] -> []
                            x  -> x
