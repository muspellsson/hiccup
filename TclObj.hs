module TclObj where

import Test.HUnit  -- IGNORE

import qualified BSParse as P
import qualified Data.ByteString.Char8 as BS

type BString = BS.ByteString

data TclObj = TclInt !Int BString | 
              TclBStr !BString (Maybe Int) (Either String [[P.TclWord]]) deriving (Show,Eq)

mkTclStr s  = mkTclBStr (BS.pack s)
mkTclBStr s = mkTclBStrP s (P.runParse s)

mkTclBStrP s res = TclBStr s mayint (resultToEither res s)
  where mayint = case BS.readInt (P.dropWhite s) of
                   Nothing -> Nothing
                   Just (i,_) -> Just i

resultToEither :: Maybe ([[P.TclWord]], BString) -> BString -> Either String [[P.TclWord]]
resultToEither res s = case res of
                        Nothing -> Left $ "parse failed: " ++ show s
                        Just (r,rs) -> if BS.null rs then Right r else Left ("Incomplete parse: " ++ show rs)

mkTclInt i = TclInt i bsval
 where bsval = BS.pack (show i)
{-# INLINE mkTclInt #-}

empty = TclBStr BS.empty Nothing (Left "bad parse")

tclTrue = mkTclInt 1 
tclFalse = mkTclInt 0 

fromBool b = if b then tclTrue else tclFalse

trim :: TclObj -> BString
trim = BS.reverse . P.dropWhite . BS.reverse . P.dropWhite . asBStr

class ITObj o where
  asStr :: o -> String
  asBool :: o -> Bool
  asInt :: (Monad m) => o -> m Int
  asBStr :: o -> BString
  asParsed :: (Monad m) => o -> m [[P.TclWord]]

instance ITObj BS.ByteString where
  asStr = BS.unpack
  asBool bs = (bs `elem` trueValues)
  asInt bs = maybe (fail ("Bad int: " ++ show bs)) (return . fst) (BS.readInt bs)
  asBStr = id
  asParsed s = either fail return (resultToEither (P.runParse s) s)

instance ITObj TclObj where
  asStr (TclInt _ b) = BS.unpack b
  asStr (TclBStr bs _ _) =  BS.unpack bs

  asBool (TclInt i _) = i /= 0
  asBool (TclBStr bs _ _) = bs `elem` trueValues

  asInt (TclInt i _) = return i
  asInt (TclBStr _ (Just i) _) = return i
  asInt (TclBStr v Nothing _) = fail $ "Bad int: " ++ show v
  
  asBStr (TclBStr s _ _) = s
  asBStr (TclInt _ b) = b

  asParsed (TclBStr _ _ (Left f))  = fail f
  asParsed (TclBStr _ _ (Right r)) = return r
  asParsed (TclInt _ _) = fail "Can't parse an int value"
  

asList :: (Monad m, ITObj o) => o -> m [BString]
asList obj = case P.parseList (asBStr obj) of
               Nothing  -> fail $ "list parse failure: " ++ show (asBStr obj)
               Just lst -> return lst


trueValues = map BS.pack ["1", "true", "yes", "on"]

-- # TESTS # --

testTrim = TestList [
     trim (int 10) ?=> "10",
     trim (str "10") ?=> "10",
     trim (str "     10 ") ?=> "10",
     trim (str "\t10\t") ?=> "10",
     trim (str "  \t    ") ?=> "",
     trim (str "     10 ") ?=> "10"
   ]
 where (?=>) s e = s ~=? (BS.pack e)
       int i = mkTclInt i
       str s = mkTclStr s

testAsBool = TestList [
   tclTrue `is` True,
   tclFalse `is` False,
   (fromBool True) `is` True,
   (int 1) `is` True,
   (int 0) `is` False,
   (int 2) `is` True,
   (int (-1)) `is` True,
   (str "true") `is` True,
   (str "yes") `is` True,
   (str "on") `is` True,
   (fromBool False) `is` False
  ]
 where is a b = asBool a ~=? b
       int i = mkTclInt i
       str s = mkTclStr s

tclObjTests = TestList [ testTrim, testAsBool ]

-- # ENDTESTS # --
