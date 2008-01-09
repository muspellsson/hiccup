{-# OPTIONS_GHC -fbang-patterns #-}
module Hash where

import qualified Data.ByteString.Char8 as B
import Data.Bits
import Data.Word
import Data.Char (ord)

main = B.getContents >>= print . perlhash

perlhash :: B.ByteString -> Word32
perlhash s = v2 + (v2 `shiftL` 15)
 where v0 = B.foldl' pcombine 0 s
       v1 = v0 + (v0 `shiftL` 3)
       v2    = v1 `xor` (v1 `shiftR` 11)

pcombine :: Word32 -> Char -> Word32
pcombine !val !c = let v1 = val + (fromIntegral (ord c)) 
                   in let v2 = v1 + v1 `shiftL` 10 
                      in v2 `xor` (v2 `shiftR` 6)
{-# INLINE pcombine #-}


