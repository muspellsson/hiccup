module VarName (parseVarName, 
                parseNS,
                parseProc,
                VarName(..), 
                showVN, 
                NSRef(..), 
                NSTag(..),
                isGlobal,
                isLocal,
                explodeNS,
                splitWith,
                nsSep,
                varNameTests) where
import Util
import qualified Data.ByteString.Char8 as B
import Test.HUnit

data NSRef a = NSRef NSTag a deriving (Eq,Show)

data NSTag = NS [BString] | Local deriving (Eq,Show)

data VarName = VarName { vnName :: !BString, vnInd :: Maybe BString } deriving (Eq,Show)

nsSep = pack "::"

explodeNS bstr = bstr `splitWith` nsSep
{-# INLINE explodeNS #-}

isGlobal (NS [x]) = B.null x
isGlobal _        = False
{-# INLINE isGlobal #-}

isLocal Local = True
isLocal _     = False
{-# INLINE isLocal #-}

parseVarName n = 
   let (name,ind) = parseArrRef n 
   in case parseNS name of
       Left _       -> NSRef Local (VarName name ind)
       Right (ns,n) -> NSRef (NS ns) (VarName n ind)

parseProc name =
   case parseNS name of
     Left _       -> NSRef Local name
     Right (ns,n) -> NSRef (NS ns) n

showVN :: VarName -> String
showVN (VarName name Nothing) = show name
showVN (VarName name (Just i)) = "\"" ++ unpack name ++ "(" ++ unpack i ++ ")\""

parseArrRef str = case B.elemIndex '(' str of
             Nothing    -> (str, Nothing)
             Just start -> if (start /= 0) && B.last str == ')' 
                             then let (pre,post) = B.splitAt start str
                                  in (pre, Just (B.tail (B.init post)))
                             else (str, Nothing)

parseNS str = 
  case explodeNS str of
    [str] -> Left str
    nsr   -> let (n:rx) = reverse nsr 
             in Right (reverse rx, n)
{-# INLINE parseNS #-}

splitWith :: BString -> BString -> [BString]
splitWith str sep = 
    case B.findSubstrings sep str of
        []     -> [str]
        il     -> extract il str
 where slen             = B.length sep 
       extract [] s     = [s]
       extract (i:ix) s = let (b,a) = B.splitAt i s 
                          in b : extract (map (\v -> v - (i+slen)) ix) (B.drop slen a)
{-# INLINE splitWith #-}
 
varNameTests = TestList [splitWithTests, testArr, testParseVarName, testParseNS] where 
  bp = pack
  splitWithTests = TestList [
      ("one::two","::") `splitsTo` ["one","two"]
      ,("::x","::") `splitsTo` ["","x"]
      ,("wonderdragon","::") `splitsTo` ["wonderdragon"]
      ,("","::") `splitsTo` [""]
      ,("::","::") `splitsTo` ["", ""]
    ]
   where splitsTo (a,b) r = map bp r ~=? ((bp a) `splitWith` (bp b))

  testParseNS = TestList [
     parseNS (bp "boo") ~=? Left (bp "boo") 
     ,parseNS (bp "::boo") ~=? Right ([B.empty], bp "boo") 
     ,parseNS (bp "foo::boo") ~=? Right ([bp "foo"], bp "boo") 
     ,parseNS (bp "::foo::boo") ~=? Right ([bp "", bp "foo"], bp "boo") 
     ,parseNS (bp "woo::foo::boo") ~=? Right ([bp "woo", bp "foo"], bp "boo") 
   ]

  testParseVarName = TestList [
      parseVarName (bp "x") ~=? NSRef Local (VarName (bp "x") Nothing)
      ,parseVarName (bp "x(a)") ~=? NSRef Local (VarName (bp "x") (Just (bp "a")))
      ,parseVarName (bp "::x") ~=? NSRef (NS [bp ""]) (VarName (bp "x") Nothing)
    ]

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
