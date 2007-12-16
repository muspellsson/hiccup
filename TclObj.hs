module TclObj where

import qualified Data.ByteString.Char8 as BS

type Str = String

data TclObj = TclInt !Int BS.ByteString | TclBStr !BS.ByteString (Maybe Int) deriving (Show,Eq)

mkTclStr s = mkTclBStr (BS.pack s)
mkTclBStr s = TclBStr s mayint 
  where mayint = case BS.readInt s of
                   Nothing -> Nothing
                   Just (i,_) -> Just i
                  
mkTclInt i = TclInt i (BS.pack (show i))

empty = TclBStr BS.empty Nothing

class ITObj o where
  asStr :: o -> Str
  asBool :: o -> Bool
  asInt :: (Monad m) => o -> m Int
  asBStr :: o -> BS.ByteString

instance ITObj BS.ByteString where
  asStr = BS.unpack
  asBool = (`elem` trueValues)
  asInt bs = maybe (fail ("Bad int: " ++ show bs)) (return . fst) (BS.readInt bs)
  asBStr = id

instance ITObj TclObj where
  asStr (TclInt i b) = BS.unpack b
  asStr (TclBStr bs _) = asStr bs

  asBool (TclInt i _) = i /= 0
  asBool (TclBStr bs _) = asBool bs

  asInt (TclInt i _) = return i
  asInt (TclBStr v (Just i)) = return i
  asInt (TclBStr v Nothing) = fail $ "Bad int: " ++ show v
  
  asBStr (TclBStr s _) = s
  asBStr (TclInt _ b) = b
  

trueValues = map BS.pack ["1", "true", "yes", "on"]

