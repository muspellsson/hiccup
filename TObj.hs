module TObj where

import Util
import qualified Data.Sequence as S

class ITObj o where
  asBool :: (Monad m) => o -> m Bool
  asInt :: (Monad m) => o -> m Int 
  asDouble :: (Monad m) => o -> m Double
  asBStr :: o -> BString
  asSeq   :: (Monad m) => o -> m (S.Seq o)
  fromInt :: Int -> o
  fromDouble :: Double -> o
  fromBStr :: BString -> o
  fromStr :: String -> o
  fromBool :: Bool -> o
  strEq :: o -> o -> Bool
  strNe :: o -> o -> Bool
