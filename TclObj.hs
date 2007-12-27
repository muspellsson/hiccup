module TclObj where

import qualified BSParse as P
import System.IO
import Data.Unique

import qualified Data.ByteString.Char8 as BS

data TclChan = TclChan { chanHandle :: Handle, chanName :: BS.ByteString } deriving (Eq,Show)

data TclObj = TclInt !Int BS.ByteString | 
              TclBStr !BS.ByteString (Maybe Int) (Either String [[P.TclWord]]) deriving (Show,Eq)

mkTclStr s = mkTclBStr (BS.pack s)
mkTclBStr s = mkTclBStrP s (doParse s)

mkTclBStrP s p = TclBStr s mayint p
  where mayint = case BS.readInt (P.dropWhite s) of
                   Nothing -> Nothing
                   Just (i,_) -> Just i

tclStdout = mkChan' stdout "stdout"
tclStdin = mkChan' stdin "stdin"
tclStderr = mkChan' stderr "stderr"


mkChan h = do n <- uniqueNum
              return (mkChan' h ("file" ++ show n))
 where uniqueNum = newUnique >>= return . hashUnique

mkChan' h n = TclChan h (BS.pack n)

doParse s = case P.runParse s of
                 Nothing -> Left "parse failed"
                 Just (r,rs) -> if BS.null rs then Right r else Left ("Incomplete parse: " ++ show rs)
                  
mkTclInt i = TclInt i (BS.pack (show i))

empty = TclBStr BS.empty Nothing (Left "bad parse")

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
                 Just (r,rs) -> if BS.null rs then return r
                                              else fail $ "incomplete parse: " ++ show rs

instance ITObj TclObj where
  asStr (TclInt _ b) = BS.unpack b
  asStr (TclBStr bs _ _) = asStr bs

  asBool (TclInt i _) = i /= 0
  asBool (TclBStr bs _ _) = asBool bs

  asInt (TclInt i _) = return i
  asInt (TclBStr _ (Just i) _) = return i
  asInt (TclBStr v Nothing _) = fail $ "Bad int: " ++ show v
  
  asBStr (TclBStr s _ _) = s
  asBStr (TclInt _ b) = b

  asParsed (TclBStr _ _ (Left f)) = fail f
  asParsed (TclBStr _ _ (Right r)) = return r
  asParsed (TclInt _ _) = fail "Can't parse an int value"
  

trueValues = map BS.pack ["1", "true", "yes", "on"]
