{-# LANGUAGE BangPatterns #-}
module TclObj (
 TclObj
 ,ITObj
 ,fromInt
 ,fromDouble
 ,mkTclStr
 ,mkTclBStr
 ,mkTclList
 ,mkTclList'
 ,mkTclInt
 ,mkTclDouble
 ,fromBlock
 ,fromBool
 ,empty
 ,isEmpty
 ,tclTrue
 ,tclFalse
 ,asStr
 ,asBool
 ,asInt
 ,asBStr
 ,asParsed
 ,asSeq
 ,asList
 ,asDouble
 ,(.==)
 ,strEq
 ,strNe
 ,trim
 ,objconcat
 ,tclObjTests ) where

import TclParse (parseList)
import qualified Data.ByteString.Char8 as BS
import Control.Monad
import RToken (Parsed, asParsed, tryParsed, singleTok, Parseable)
import Util
import qualified Data.Sequence as S
import qualified Data.Foldable as F

import Test.HUnit

data TclObj = TclInt !Int BString |
              TclDouble !Double BString |
              TclList !(S.Seq TclObj) BString |
              TclBStr !BString (Maybe Int) (Either String Parsed) deriving (Show,Eq)

mkTclStr s  = mkTclBStr (pack s)
mkTclBStr s = TclBStr s (maybeInt s) (tryParsed s)
{-# INLINE mkTclBStr #-}

mkTclList l  = TclList (S.fromList l) (fromList (map asBStr l))
mkTclList' l = TclList l (fromList (map asBStr (F.toList l)))

fromBlock s p = TclBStr s (maybeInt s) p
{-# INLINE fromBlock #-}

maybeInt s = case BS.readInt (dropSpaces s) of
                Nothing    -> Nothing
                Just (i,r) -> if BS.null r || BS.null (dropSpaces r) then Just i else Nothing

strEq (TclInt i1 _) (TclInt i2 _) = i1 == i2
strEq o1            o2            = asBStr o1 == asBStr o2 

strNe (TclInt i1 _) (TclInt i2 _) = i1 /= i2
strNe o1            o2            = asBStr o1 /= asBStr o2 


mkTclInt !i = TclInt i bsval
 where bsval = pack (show i)
{-# INLINE mkTclInt #-}

mkTclDouble :: Double -> TclObj
mkTclDouble !d = TclDouble d bsval
 where bsval = pack (show d)

empty = TclBStr BS.empty Nothing (Left "bad parse")
isEmpty (TclInt _ _) = False
isEmpty (TclDouble _ _) = False
isEmpty v = BS.null (asBStr v)


tclTrue = mkTclInt 1
tclFalse = mkTclInt 0


trim = BS.reverse . dropSpaces . BS.reverse . dropSpaces

class ITObj o where
  asBool :: o -> Bool
  asInt :: (Monad m) => o -> m Int 
  asDouble :: (Monad m) => o -> m Double
  asBStr :: o -> BString
  asSeq   :: (Monad m) => o -> m (S.Seq o)
  fromInt :: Int -> o
  fromDouble :: Double -> o
  fromBStr :: BString -> o
  fromBool :: Bool -> o

bstrAsInt bs = case BS.readInt bs of
               Nothing    -> fail ("Bad int: " ++ show bs)
               Just (i,r) -> if BS.null r then return i else fail ("Bad int: " ++ show bs)

bstrAsSeq s = case parseList s of
                    Left r  -> fail $ "list parse failure: " ++ r
                    Right lst -> return (S.fromList lst)
{-
instance ITObj BS.ByteString where
  asStr = BS.unpack
  asBool bs = (bs `elem` trueValues)
  asInt = bstrAsInt
  asBStr = id
  asParsed s = either fail return (resultToEither (P.runParse s) s)
  asSeq st = liftM S.fromList (asListS st)

-}

asList :: (Monad m) => TclObj -> m [TclObj]
asList o = liftM F.toList (asSeq o)

asStr o = unpack (asBStr o)

instance ITObj TclObj where
  asBool (TclList _ bs)   = bs `elem` trueValues
  asBool (TclInt i _)     = i /= 0
  asBool (TclDouble d _)  = d /= 0.0
  asBool (TclBStr bs _ _) = bs `elem` trueValues
  {-# INLINE asBool #-}

  asInt (TclInt i _) = return i
  asInt (TclDouble _ b) = fail $ "expected integer, got " ++ show b
  asInt (TclBStr _ (Just i) _) = return i
  asInt (TclBStr v Nothing _) = fail $ "Bad int: " ++ show v
  asInt (TclList _ v)         = bstrAsInt v
  {-# INLINE asInt #-}

  asDouble (TclDouble d _) = return $! d
  asDouble (TclInt i _) = return $! (fromIntegral i)
  asDouble obj = do
      case asInt obj of 
        Just i  -> return $! (fromIntegral i)
        Nothing -> let strval = asStr obj 
                   in case reads strval of
                     [(d,"")] -> return $! d -- TODO: not quite right.
                     _ -> fail $ "expected float but got " ++ show strval

  {-# INLINE asDouble #-}

  asBStr (TclBStr s _ _) = s
  asBStr (TclInt _ b) = b
  asBStr (TclDouble _ b) = b
  asBStr (TclList _ b) = b
  {-# INLINE asBStr #-}

  asSeq (TclBStr s _ _) = bstrAsSeq s >>= return . fmap mkTclBStr
  asSeq (TclList l _)   = return l
  asSeq v               = return (S.singleton v)

  fromInt = mkTclInt
  fromDouble = mkTclDouble
  fromBStr = mkTclBStr
  fromBool !b = if b then tclTrue else tclFalse
  {-# INLINE fromBool #-}

instance Parseable TclObj where
  asParsed (TclBStr _ _ (Left f))  = fail f
  asParsed (TclBStr _ _ (Right r)) = return r
  asParsed (TclInt _ b) = return (singleTok b)
  asParsed (TclDouble _ b)  = return (singleTok b)
  asParsed (TclList _ s) = asParsed s
  {-# INLINE asParsed #-}


fromList l = (map listEscape l)  `joinWith` ' '



trueValues = map pack ["1", "true", "yes", "on"]

(.==) :: TclObj -> String -> Bool
(.==) bs str = (asBStr bs) == pack str
{-# INLINE (.==) #-}

objconcat :: [TclObj] -> TclObj
objconcat = mkTclBStr . (`joinWith` ' ') . map (trim . asBStr) 

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
 where is :: TclObj -> Bool -> Test
       is a b = asBool a ~=? b
       int i = mkTclInt i
       str s = mkTclStr s

tclObjTests = TestList [ testTrim, testAsBool ]

-- # ENDTESTS # --
