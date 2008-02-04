{-# LANGUAGE BangPatterns #-}
module TclObj (
 TclObj
 ,mkTclStr
 ,mkTclBStr
 ,mkTclList
 ,mkTclList'
 ,mkTclInt
 ,fromBlock
 ,fromBool
 ,empty
 ,tclTrue
 ,tclFalse
 ,ITObj
 ,asStr
 ,asBool
 ,asInt
 ,asBStr
 ,asParsed
 ,asSeq
 ,asList
 ,(.==)
 ,trim
 ,tclObjTests ) where

import qualified BSParse as P
import qualified Data.ByteString.Char8 as BS
import Control.Monad
import RToken
import Util
import qualified Data.Sequence as S
import qualified Data.Foldable as F

import Test.HUnit

type Parsed = [Cmd]
data TclObj = TclInt !Int BString |
              TclList !(S.Seq TclObj) BString |
              TclBStr !BString (Maybe Int) (Either String Parsed) deriving (Show,Eq)

mkTclStr s  = mkTclBStr (pack s)
mkTclBStr s = mkTclBStrP s (P.runParse s)

mkTclList l  = TclList (S.fromList l) (fromList (map asBStr l))
mkTclList' l = TclList l (fromList (map asBStr (F.toList l)))

fromBlock s p = TclBStr s (maybeInt s) p

maybeInt s = case BS.readInt (P.dropWhite s) of
                Nothing    -> Nothing
                Just (i,r) -> if BS.null r || BS.null (P.dropWhite r) then Just i else Nothing

mkTclBStrP s res = TclBStr s (maybeInt s) (resultToEither res s)

resultToEither :: P.Result -> BString -> Either String Parsed
resultToEither res s = case res of
                        Nothing -> Left $ "parse failed: " ++ show s
                        Just (r,rs) -> if BS.null rs then Right (map toCmd r) else Left ("Incomplete parse: " ++ show rs)

mkTclInt !i = TclInt i bsval
 where bsval = pack (show i)
{-# INLINE mkTclInt #-}

empty = TclBStr BS.empty Nothing (Left "bad parse")

tclTrue = mkTclInt 1
tclFalse = mkTclInt 0

fromBool !b = if b then tclTrue else tclFalse

trim = BS.reverse . P.dropWhite . BS.reverse . P.dropWhite

class ITObj o where
  asStr :: o -> String
  asBool :: o -> Bool
  asInt :: (Monad m) => o -> m Int
  asBStr :: o -> BString
  asParsed :: (Monad m) => o -> m Parsed
  asSeq   :: (Monad m) => o -> m (S.Seq o)


instance ITObj BS.ByteString where
  asStr = BS.unpack
  asBool bs = (bs `elem` trueValues)
  asInt bs = case BS.readInt bs of
               Nothing    -> fail ("Bad int: " ++ show bs)
               Just (i,r) -> if BS.null r then return i else fail ("Bad int: " ++ show bs)
  asBStr = id
  asParsed s = either fail return (resultToEither (P.runParse s) s)
  asSeq st = liftM S.fromList (asListS st)

asList o = liftM F.toList (asSeq o)

instance ITObj TclObj where
  asStr (TclInt _ b)     = unpack b
  asStr (TclBStr bs _ _) = unpack bs
  asStr (TclList _ bs)   = unpack bs

  asBool (TclList _ bs) = bs `elem` trueValues
  asBool (TclInt i _)     = i /= 0
  asBool (TclBStr bs _ _) = bs `elem` trueValues

  asInt (TclInt i _) = return i
  asInt (TclBStr _ (Just i) _) = return i
  asInt (TclBStr v Nothing _) = fail $ "Bad int: " ++ show v
  asInt (TclList _ v)         = asInt v

  asBStr (TclBStr s _ _) = s
  asBStr (TclInt _ b) = b
  asBStr (TclList _ b) = b
  {-# INLINE asBStr #-}

  asSeq i@(TclInt _ _) = return (S.singleton i)
  asSeq (TclBStr s _ _) = asSeq s >>= return . fmap mkTclBStr
  asSeq (TclList l _)   = return l

  asParsed (TclBStr _ _ (Left f))  = fail f
  asParsed (TclBStr _ _ (Right r)) = return r
  asParsed (TclInt _ _)  = fail "Can't parse an int value"
  asParsed (TclList _ _) = fail "Can't parse a list value (for now)"

fromList l = (map listEscape l)  `joinWith` ' '

asListS s = case P.parseList s of
               Nothing  -> fail $ "list parse failure: " ++ show s
               Just lst -> return lst


trueValues = map pack ["1", "true", "yes", "on"]

(.==) :: TclObj -> String -> Bool
(.==) bs str = (asBStr bs) == pack str
{-# INLINE (.==) #-}

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
       int = asBStr . mkTclInt
       str = asBStr . mkTclStr

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
