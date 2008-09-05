{-# LANGUAGE BangPatterns,OverloadedStrings #-}
module VarName (parseVarName, 
                nsTail,
                nsQualifiers,
                parseNSTag,
                parseProc,
                VarName(..), 
                arrName,
                arrNameNS,
                isArr,
                showVN, 
                NSQual(..), 
                NSTag(..),
                asGlobal,
                isGlobalQual,
                noNsQual,
                splitWith,
                nsSep,
                BStringable(..),
                varNameTests) where
import Util
import qualified Data.ByteString.Char8 as B
import Test.HUnit


data NSQual a = NSQual !(Maybe NSTag) !a deriving (Eq,Ord,Show)

data NSTag = NS !Bool [BString] deriving (Eq,Show,Ord)

data VarName = VarName { vnName :: !BString, vnInd :: Maybe BString } deriving (Eq,Show)

class BStringable a where
  toBStr :: a -> BString

instance BStringable B.ByteString where
  toBStr = id
instance BStringable NSTag where
  toBStr (NS g lst) = B.append (if g then nsSep else B.empty) (B.intercalate nsSep lst)

instance BStringable VarName where
  toBStr (VarName n Nothing) = n
  toBStr (VarName n (Just ind)) = B.concat [n, B.singleton '(', ind, B.singleton ')']
instance (BStringable a) => BStringable (NSQual a) where
  toBStr (NSQual (Just t) s) = let pre = toBStr t 
                               in if nsSep `B.isSuffixOf` pre 
                                      then B.append (toBStr t) (toBStr s)
                                      else B.concat [toBStr t, nsSep, toBStr s]
  toBStr (NSQual _ s) = toBStr s


arrName !an !ind = VarName an (Just ind)
{-# INLINE arrName #-}

arrNameNS !an !ind = (parseNSQual an) `withIndex` ind
 where withIndex (NSQual mt s) ind = NSQual mt (VarName s (Just ind))
{-# INLINE arrNameNS #-}

isArr (VarName _ (Just _)) = True
isArr _                    = False

nsSep :: BString
nsSep = "::"

isGlobalQual (Just (NS v _)) = v
isGlobalQual _               = False
{-# INLINE isGlobalQual #-}

noNsQual nst = case nst of
         Nothing              -> True
         Just (NS False [])   -> True
         _                    -> False
{-# INLINE noNsQual #-}

asGlobal (Just (NS _ lst)) = Just (NS True lst)
asGlobal Nothing           = Just (NS True [])

parseNSQual ns = case parseNSTag ns of
                  NS False [s] -> NSQual Nothing s
                  NS gq []     -> NSQual (Just (NS gq [])) ""
                  NS gq nsl    -> NSQual (Just (NS gq (init nsl))) (last nsl)
{-# INLINE parseNSQual #-}

	       
parseNSTag ns = toNSTag (ns `splitWith` nsSep) where
   toNSTag nl = if isAbs then NS True (tail nl)
                         else NS False nl
   isAbs = nsSep `B.isPrefixOf` ns
              
parseVarName name = 
   case parseArrRef name of
     (str,ind) -> if B.notElem ':' str
                     then NSQual Nothing (VarName str ind) 
                     else case parseNSQual str of
                             NSQual nst n -> NSQual nst (VarName n ind)
{-# INLINE parseVarName #-}

parseProc :: BString -> NSQual BString
parseProc = parseNSQual
{-# INLINE parseProc #-}

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

nsTail_ x = nsTail (parseNSTag x)


nsQualifiers str = case findSubstringEnd nsSep str of
                      Nothing -> B.empty
                      Just i -> B.take i str

findSubstringEnd pat hay = search (B.length hay) hay
 where patlen = B.length pat 
       search !n !s 
        | n < patlen           = Nothing
        | pat `B.isSuffixOf` s = Just (n - patlen)
        | otherwise            = search (n-1) (B.take (n-1) s)

splitWith :: BString -> BString -> [BString]
splitWith str sep = splitter str
 where slen        = B.length sep 
       splitter !s = let (h,t) = B.breakSubstring sep s
                    in if B.null t then [h] else h : splitter (B.drop slen t)
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
    where parses_to a nst = ("parseNSTag " ++ show a) ~: nst ~=? parseNSTag a 

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

  testInvert = TestList $ doInvert (parseNSTag, tag_vals) ++ doInvert (parseProc, cmd_vals)
    where tag_vals = ["boo", "::boo", "::", "::a::b", "a::b", "", "::tcl::mathfunc::bool"]
          cmd_vals = ["boo", "::boo", "::tcl::mathfunc::bool"]
          doInvert (f,l) = map (invert f) (map bp l)  
          invert f v = v ~=? toBStr ((f v))

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
