{-# LANGUAGE BangPatterns,OverloadedStrings #-}
module TclLib.LibUtil where

import Common
import qualified Data.ByteString.Char8 as B
import qualified TclObj as T

vArgErr s = argErr ("should be " ++ show s)

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
