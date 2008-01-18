module Util where
import qualified Data.ByteString.Char8 as B
import Data.List(intersperse)

type BString = B.ByteString

joinWith bsl c = B.concat (intersperse (B.singleton c) bsl)
{-# INLINE joinWith #-}
pack = B.pack
{-# INLINE pack #-}
