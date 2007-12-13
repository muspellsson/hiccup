module TclObj where

import qualified Data.ByteString.Char8 as BS

type Str = String

data TclObj = TclStr Str | TclInt !Int deriving (Show,Eq)

class ITObj o where
  asStr :: o -> Str
  asBool :: o -> Bool
  asInt :: (Monad m) => o -> m Int

instance ITObj BS.ByteString where
  asStr = BS.unpack
  asBool = (`elem` bsTrueValues)
  asInt = parseInt

parseInt bsi = maybe (fail ("Bad int: " ++ show bsi)) (return . fst) (BS.readInt bsi)

asBStr :: (ITObj o) => o -> BS.ByteString
asBStr = BS.pack . asStr

instance ITObj TclObj where
  asStr (TclStr s) = s
  asStr (TclInt i) = show i

  asBool (TclStr s) = s `elem` trueValues
  asBool (TclInt i) = i /= 0

  asInt (TclStr s) = return (read s)
  asInt (TclInt i) = return i
  

trueValues = ["1", "true", "yes", "on"]
bsTrueValues = map BS.pack trueValues

