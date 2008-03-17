{-# LANGUAGE BangPatterns,OverloadedStrings #-}
module VarName (parseVarName, 
                nsTail,
                nsQualifiers,
                parseNSTag,
                parseProc,
                VarName(..), 
                arrName,
                isArr,
                showVN, 
                NSQual(..), 
                NSTag(..),
                asGlobal,
                isGlobalQual,
                noNsQual,
                splitWith,
                nsSep,
                varNameTests) where
import Util
import qualified Data.ByteString.Char8 as B
import Data.ByteString (findSubstrings)
import Test.HUnit


data NSQual a = NSQual !(Maybe NSTag) !a deriving (Eq,Show)

data NSTag = NS !Bool ![BString] deriving (Eq,Show)

data VarName = VarName { vnName :: !BString, vnInd :: Maybe BString } deriving (Eq,Show)

class BStringable a where
  toBStr :: a -> BString

instance BStringable NSTag where
  toBStr (NS g lst) = B.append (if g then nsSep else B.empty) (B.intercalate nsSep lst)

arrName !an !ind = VarName an (Just ind)
{-# INLINE arrName #-}

isArr (VarName _ (Just _)) = True
isArr _                    = False

nsSep :: BString
nsSep = "::"

isGlobalQual (Just (NS v _)) = v
isGlobalQual _               = False
{-# INLINE isGlobalQual #-}

noNsQual nst = case nst of
         Nothing              -> True
         (Just (NS False [])) -> True
         _                    -> False
{-# INLINE noNsQual #-}

asGlobal (Just (NS _ lst)) = Just (NS True lst)
asGlobal Nothing           = Just (NS True [])

parseNSQual ns = case parseNSTag ns of
                  Nothing -> NSQual Nothing ""
                  Just (NS False [s]) -> NSQual Nothing s
                  Just (NS gq []) -> NSQual (Just (NS gq [])) ""
                  Just (NS gq nsl) -> NSQual (Just (NS gq (init nsl))) (last nsl)

	       
parseNSTag ns = toNSTag (ns `splitWith` nsSep) where
   toNSTag nl = if isAbs then return (NS True (tail nl))
                         else return (NS False nl)
   isAbs = nsSep == B.take 2 ns 
              
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

  testParseNSQual = TestList [
       "boo" `parses_to` (Nothing, "boo")
       ,"::boo" `parses_to` (mkns True [], "boo")
       ,"::boo::flag" `parses_to` (mkns True ["boo"], "flag")
       ,"foo::bar" `parses_to` (mkns False ["foo"], "bar")
    ]
   where parses_to a (b1,b2) = ("parseNSQual " ++ show a) ~: NSQual b1 (bp b2) ~=? parseNSQual a
         mkns g v = Just (NS g v)

  testParseNS = TestList [
     "boo" `parses_to` (NS False ["boo"]) 
     ,"::boo" `parses_to` (NS True ["boo"]) 
     ,"::" `parses_to` (NS True [""]) 
     ,"foo::boo" `parses_to`  NS False ["foo", "boo"] 
     ,"::foo::boo" `parses_to` NS True ["foo", "boo"] 
     ,"woo::foo::boo" `parses_to` NS False ["woo", "foo", "boo"] 
   ]
    where parses_to a nst = ("parseNSTag " ++ show a) ~: (Just nst) ~=? parseNSTag a 

  testParseVarName = TestList [
      parseVarName "x" ~=? NSQual Nothing (VarName "x" Nothing)
      ,parseVarName "x(a)" ~=? NSQual Nothing (VarName "x" (Just "a"))
      ,parseVarName "boo::x(::)" ~=? NSQual (Just (NS False ["boo"])) (VarName "x" (Just "::"))
      ,parseVarName "::x" ~=? NSQual (Just (NS True [])) (VarName "x" Nothing)
    ]

  testNSTail = TestList [
       nsTail_ "boo" ~=? "boo"
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
     "boo" `parses_to` (NSQual Nothing "boo")
     ,"::boo" `parses_to` (NSQual (Just (NS True [])) "boo")
     ,"::some::boo" `parses_to` (NSQual (Just (NS True ["some"])) "boo")
   ]
   where parses_to a b = b ~=? parseProc (bp a)

  testInvert = TestList (map tryInvert vals)
    where vals = map bp ["boo", "::boo", "::", "::a::b", "a::b", ""]
          tryInvert v = v ~=? toBStr (up (parseNSTag v))
          up (Just x) = x
          up _        = error "Error in testInvert"

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
