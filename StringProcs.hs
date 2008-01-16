module StringProcs (stringProcs, stringTests ) where

import Common
import qualified Data.ByteString.Char8 as B
import qualified TclObj as T
import Data.Char (toLower,toUpper)

import Test.HUnit

stringProcs = makeProcMap [("string", procString), ("append", procAppend)]

retmod f = \v -> treturn (f `onObj` v)
onObj f o = (f (T.asBStr o))

procString :: TclProc
procString (f:s:args) 
 | f .== "trim"    = treturn (T.trim s)
 | f .== "tolower" = retmod (B.map toLower) s
 | f .== "toupper" = retmod (B.map toUpper) s
 | f .== "reverse" = retmod (B.reverse) s
 | f .== "length"  = return $ T.mkTclInt (B.length `onObj` s)
 | f .== "match"   = string_Match (s:args)
 | f .== "index"   = string_Index (s:args)
 | otherwise       = tclErr $ "Can't do string action: " ++ show f
 
procString _ = argErr "string"


string_Match args = case map T.asBStr args of
   [s1,s2]        -> domatch False s1 s2
   [nocase,s1,s2] -> if nocase == B.pack "-nocase" then domatch True s1 s2 else argErr "string"
   _              -> argErr "string match"
 where domatch nocase a b = return (T.fromBool (match nocase a b))

string_Index args = case args of
                     [s,i] -> do ind <- toInd s i
                                 if ind >= (B.length `onObj` s) || ind < 0 
                                  then ret 
                                  else treturn $ B.singleton (B.index (T.asBStr s) ind)
                     _   -> argErr "string index"
 where toInd s i = (T.asInt i) `orElse` tryEnd s i
       tryEnd s i = if i .== "end" then return ((B.length `onObj` s) - 1) else tclErr "bad index"

match :: Bool -> BString -> BString -> Bool
match nocase pat str = inner 0 0
 where slen = B.length str 
       plen = B.length pat
       ceq a b = if nocase then toLower a == toLower b else a == b
       inner pi si  
        | pi == plen = si == slen
        | otherwise = case B.index pat pi of
                       '*'  -> pi == (plen - 1) || or (map (inner (succ pi)) [si..(slen - 1)])
                       '?'  -> not (si == slen) && inner (succ pi) (succ si)
                       '\\' -> inner (succ pi) si
                       v    -> not (si == slen) && v `ceq` (B.index str si) && inner (succ pi) (succ si)

procAppend args = case args of
            (v:vx) -> do val <- varGet (T.asBStr v) `ifFails` T.empty
                         let cated = oconcat (val:vx)
                         varSet (T.asBStr v) cated >> return cated
            _  -> argErr "append"
 where oconcat = T.mkTclBStr . B.concat . map T.asBStr

stringTests = TestList [ matchTests ]
matchTests = TestList [
    "boo" `matches` "boo" 
    ,"" `matches` "" 
    ,"1" `matches` "1" 
    ,"a?c" `matches` "abc" 
    ,"a?c" `doesnt_match` "ab" 
    ,"a" `doesnt_match` "ab" 
    ,"ab" `doesnt_match` "a" 
    ,"a*" `matches` "abcde" 
    ,"a*b" `matches` "ab" 
    ,"a*b" `matches` "a OMG b" 
    ,"*b" `matches` "in the land of bob" 
    ,"*" `matches` "in the land of bob" 
    ,"*" `matches` ""
    ,"s\\*r" `matches` "s*r"
  ]
 where matches a b = (show a) ++ " matches " ++ (show b) ~: True ~=? match False (B.pack a) (B.pack b) 
       doesnt_match a b = (show a) ++ " doesn't match " ++ (show b) ~: False ~=? match False (B.pack a) (B.pack b)
