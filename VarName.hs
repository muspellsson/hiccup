{-# LANGUAGE BangPatterns #-}
module VarName (parseVarName, 
                nsTail,
                nsQualifiers,
                parseNS,
                parseNSTag,
                parseProc,
                VarName(..), 
                arrName,
                isArr,
                showVN, 
                NSQual(..), 
                NsName,
                NSTag(..),
                asGlobal,
                isGlobal,
                isGlobalQual,
                isLocal,
                splitWith,
                nsSep,
                varNameTests) where
import Util
import qualified Data.ByteString.Char8 as B
import Data.ByteString (findSubstrings)
import Test.HUnit

isLocal Nothing = True
isLocal _ = False

data NSQual a = NSQual !(Maybe NSTag) !a deriving (Eq,Show)

data NSTag = NS Bool ![BString] deriving (Eq,Show)

type NsName = ([BString],BString)

data VarName = VarName { vnName :: !BString, vnInd :: Maybe BString } deriving (Eq,Show)


class BStringable a where
  toBStr :: a -> BString

instance BStringable NSTag where
  toBStr (NS _ []) = (pack "")
  toBStr (NS _ lst) = (B.intercalate nsSep lst)

arrName !an !ind = VarName an (Just ind)
{-# INLINE arrName #-}

isArr (VarName _ (Just _)) = True
isArr _                    = False

nsSep = pack "::"

explodeNS bstr = bstr `splitWith` nsSep
{-# INLINE explodeNS #-}

isGlobal (Just (NS _ [x])) = B.null x
isGlobal _   = False
{-# INLINE isGlobal #-}

isGlobalQual (Just (NS _ (x:_))) = B.null x
isGlobalQual _          = False
{-# INLINE isGlobalQual #-}

asGlobal (Just (NS _ lst)) = Just (NS True (B.empty:lst))
asGlobal Nothing = Just (NS True [B.empty])

parseNSQual ns = case parseNS ns of
               (nl,n) -> NSQual (toNSTag nl) n 
	       
parseNSTag ns = toNSTag (explodeNS ns)
toNSTag [] = fail "empty namespace designator"
toNSTag nl = return (NS (B.null (head nl)) nl)
              
parseVarName name = 
   case parseArrRef name of
     (str,ind) -> case parseNSQual str of
                    NSQual nst n -> NSQual nst (VarName n ind)

parseProc :: BString -> NSQual BString
parseProc = parseNSQual

showVN :: VarName -> String
showVN (VarName name Nothing) = show name
showVN (VarName name (Just i)) = "\"" ++ unpack name ++ "(" ++ unpack i ++ ")\""

parseArrRef str = case B.elemIndex '(' str of
             Nothing    -> (str, Nothing)
             Just start -> if (start /= 0) && B.last str == ')' 
                             then let (pre,post) = B.splitAt start str
                                  in (pre, Just (B.tail (B.init post)))
                             else (str, Nothing)
nsTail (NS _ []) = error "Malformed NSTag"
nsTail (NS _ nsl) = last nsl

nsTail_ x = case parseNSTag x of
              Nothing -> error "FAIL"
	      Just v -> nsTail v


nsQualifiers str = case findSubstrings nsSep str of
                      [] -> B.empty
                      lst -> B.take (last lst) str


parseNS :: BString -> NsName
parseNS !str = 
  case explodeNS str of
    [] -> ([],B.empty) -- This should never happen..
    nsr -> let (n:rx) = reverse nsr 
           in (reverse rx, n)
{-# INLINE parseNS #-}

splitWith :: BString -> BString -> [BString]
splitWith str sep = 
    case findSubstrings sep str of
        []     -> [str]
        il     -> extract il str
 where slen              = B.length sep 
       extract [] !s     = [s]
       extract (i:ix) !s = let (b,a) = B.splitAt i s 
                          in b : extract (map (\v -> v - (i+slen)) ix) (B.drop slen a)
{-# INLINE splitWith #-}
 
varNameTests = TestList [splitWithTests, testArr, testParseVarName, testParseNS, testNSTail, 
                         testNsQuals,testInvert, testParseNSQual,
                         testParseProc] where 
  bp = pack
  splitWithTests = TestList [
      ("one::two","::") `splitsTo` ["one","two"]
      ,("::x","::") `splitsTo` ["","x"]
      ,("wonderdragon","::") `splitsTo` ["wonderdragon"]
      ,("","::") `splitsTo` [""]
      ,("::","::") `splitsTo` ["", ""]
    ]
   where splitsTo (a,b) r = map bp r ~=? ((bp a) `splitWith` (bp b))

  testParseNSQual = TestList []
  testParseNS = TestList [
     "boo" `parses_to` ([], "boo") 
     ,"::boo" `parses_to` ([""], "boo") 
     ,"foo::boo" `parses_to`  (["foo"], "boo") 
     ,"::foo::boo" `parses_to` (["", "foo"], "boo") 
     ,"woo::foo::boo" `parses_to` (["woo", "foo"], "boo") 
   ]
    where parses_to a (l,b) = (map bp l, bp b) ~=? parseNS (bp a) 

  testParseVarName = TestList [
      parseVarName (bp "x") ~=? NSQual Nothing (VarName (bp "x") Nothing)
      ,parseVarName (bp "x(a)") ~=? NSQual Nothing (VarName (bp "x") (Just (bp "a")))
      ,parseVarName (bp "boo::x(::)") ~=? NSQual (Just (NS False [bp "boo"])) (VarName (bp "x") (Just (bp "::")))
      ,parseVarName (bp "::x") ~=? NSQual (Just (NS True [bp ""])) (VarName (bp "x") Nothing)
    ]

  testNSTail = TestList [
       nsTail_ (bp "boo") ~=? (bp "boo")
       ,nsTail_ (bp "::boo") ~=? (bp "boo")
       ,nsTail_ (bp "baby::boo") ~=? (bp "boo")
       ,nsTail_ (bp "::baby::boo") ~=? (bp "boo")
       ,nsTail_ (bp "::") ~=? (bp "")
    ]
  testNsQuals = TestList [
      "boo" `should_be` ""
      ,"::" `should_be` ""
      ,"a::b" `should_be` "a"
      ,"a::b::c" `should_be` "a::b"
      ,"::a::b::c" `should_be` "::a::b"
    ]
   where should_be b a = nsQualifiers (bp b) ~=? (bp a)
  
  testParseProc = TestList [
     "boo" `parses_to` (NSQual Nothing (bp "boo"))
     ,"::boo" `parses_to` (NSQual (Just (NS True [bp ""])) (bp "boo"))
     ,"::some::boo" `parses_to` (NSQual (Just (NS True [bp "", bp "some"])) (bp "boo"))
   ]
   where parses_to a b = b ~=? parseProc (bp a)

  testInvert = TestList (map tryInvert vals)
    where vals = map bp ["boo", "::boo", "::", "::a::b", "a::b", ""]
          tryInvert v = v ~=? toBStr (up (parseNSTag v))
	  up (Just x) = x

  testArr = TestList [
     "december" `should_be` Nothing
     ,"dec(mber" `should_be` Nothing
     ,"dec)mber" `should_be` Nothing
     ,"(cujo)" `should_be` Nothing
     ,"de(c)mber" `should_be` Nothing
     ,"a(1)"          ?=> ("a","1")
     ,"boo(4)"        ?=> ("boo","4")
     ,"xx(september)" ?=> ("xx","september")
     ,"arr(3,4,5)"    ?=> ("arr","3,4,5")
     ,"arr()"         ?=> ("arr","")
   ]
   where (?=>) a b@(b1,b2) = (a ++ " -> " ++ show b) ~: parseArrRef (bp a) ~=? ((bp b1), Just (bp b2))
         should_be x _ =  (x ++ " should be " ++ show (bp x)) ~: parseArrRef (bp x) ~=? (bp x, Nothing)
