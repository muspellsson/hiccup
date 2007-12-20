module TclObj where

import qualified BSParse as P

import qualified Data.ByteString.Char8 as BS

data TclObj = TclInt !Int BS.ByteString | TclBStr !BS.ByteString (Maybe Int) (Maybe [[P.TclWord]]) deriving (Show,Eq)

mkTclStr s = mkTclBStr (BS.pack s)
mkTclBStr s = mkTclBStrP s (doParse s)

isSpace x = x == ' ' || x == '\t'

mkTclBStrP s p = TclBStr s mayint p
  where mayint = case BS.readInt (BS.dropWhile isSpace s) of
                   Nothing -> Nothing
                   Just (i,_) -> Just i



doParse s = case P.runParse s of
                 Nothing -> Nothing
                 Just (r,_) -> Just r
                  
mkTclInt i = TclInt i (BS.pack (show i))

empty = TclBStr BS.empty Nothing Nothing

tclTrue = mkTclInt 1 
tclFalse = mkTclInt 0 

fromBool b = if b then tclTrue else tclFalse

class ITObj o where
  asStr :: o -> String
  asBool :: o -> Bool
  asInt :: (Monad m) => o -> m Int
  asBStr :: o -> BS.ByteString
  asParsed :: (Monad m) => o -> m [[P.TclWord]]

instance ITObj BS.ByteString where
  asStr = BS.unpack
  asBool bs = (bs `elem` trueValues)
  asInt bs = maybe (fail ("Bad int: " ++ show bs)) (return . fst) (BS.readInt bs)
  asBStr = id
  asParsed s = case P.runParse s of
                 Nothing -> fail $ "parse failed: " ++ show s
                 Just (r,_) -> return r

instance ITObj TclObj where
  asStr (TclInt i b) = BS.unpack b
  asStr (TclBStr bs _ _) = asStr bs

  asBool (TclInt i _) = i /= 0
  asBool (TclBStr bs _ _) = asBool bs

  asInt (TclInt i _) = return i
  asInt (TclBStr v (Just i) _) = return i
  asInt (TclBStr v Nothing _) = fail $ "Bad int: " ++ show v
  
  asBStr (TclBStr s _ _) = s
  asBStr (TclInt _ b) = b

  asParsed (TclBStr _ _ Nothing) = fail "Parse failed"
  asParsed (TclBStr _ v (Just r)) = return r
  asParsed (TclInt _ _) = fail "Can't parse an int value"
  

trueValues = map BS.pack ["1", "true", "yes", "on"]


