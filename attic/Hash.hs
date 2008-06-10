{-# LANGUAGE BangPatterns #-}
module Hash (mainhash, sumhash) where

import qualified Data.ByteString.Char8 as B
import Data.Bits
import Data.Char (ord)

data Hashed a = Hashed { hshDat :: !a, hshVal :: !Int } deriving (Eq,Show)

toHashed v = Hashed v (hashVal v)

class Hashable a where
   hashVal :: a -> Int

instance Hashable (Hashed a) where
    hashVal = hshVal

instance Hashable B.ByteString where
    hashVal = mainhash

sumhash :: B.ByteString -> Int
sumhash = B.foldl' (\a b -> a + (fromIntegral (ord b))) 0

perlhash :: B.ByteString -> Int
perlhash !s = let v0 = B.foldl' pcombine 0 s
                  v1 = v0 `seq` v0 + (v0 `shiftL` 3)
                  v2 = v1 `seq` v1 `xor` (v1 `shiftR` 11)
              in  v2 + (v2 `shiftL` 15)
{-# INLINE perlhash #-}

mainhash :: B.ByteString -> Int
mainhash !s = B.foldl' (\i c -> ((i `shiftL` 5) + i) `xor` (ord c)) 5381 s
{-# INLINE mainhash #-}

pcombine :: Int -> Char -> Int
pcombine !val !c = let v1 = val + (fromIntegral (ord c))
                       v2 = v1 + v1 `shiftL` 10
                   in  v2 `xor` (v2 `shiftR` 6)
{-# INLINE pcombine #-}


