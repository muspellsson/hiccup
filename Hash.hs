{-# OPTIONS_GHC -fbang-patterns #-}
module Hash (perlhash, sumhash) where

import qualified Data.ByteString.Char8 as B
import Data.Bits
import Data.Char (ord)


sumhash :: B.ByteString -> Int
sumhash = B.foldl' (\a b -> a + (fromIntegral (ord b))) 0

perlhash :: B.ByteString -> Int
perlhash s = let v0 = B.foldl' pcombine 0 s 
                 v1 = v0 + (v0 `shiftL` 3) 
                 v2 = v1 `xor` (v1 `shiftR` 11)
             in  v2 + (v2 `shiftL` 15)

pcombine :: Int -> Char -> Int
pcombine !val !c = let v1 = val + (fromIntegral (ord c)) 
                       v2 = v1 + v1 `shiftL` 10 
                   in  v2 `xor` (v2 `shiftR` 6)
{-# INLINE pcombine #-}


