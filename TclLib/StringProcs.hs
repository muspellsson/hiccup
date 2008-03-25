module TclLib.StringProcs (stringProcs, stringTests) where

import Common
import Util
import qualified Data.ByteString.Char8 as B
import qualified TclObj as T
import TclObj ((.==))
import Data.Char (toLower,toUpper)

import Test.HUnit

stringProcs = makeCmdMap [("string", procString), ("append", procAppend), ("split", procSplit)]

procString = makeEnsemble "string" [
   ("trim", string_Op "trim" T.trim), 
   ("tolower", string_Op "tolower" (B.map toLower)),
   ("toupper", string_Op "toupper" (B.map toUpper)),
   ("reverse", string_Op "reverse" B.reverse),
   ("length", string_length),
   ("match", string_match), ("compare", string_compare),
   ("index", string_index)
 ]

string_Op name op args = case args of
   [s] -> treturn $! op (T.asBStr s)
   _   -> argErr $ "string " ++ name

string_length args = case args of
    [s] -> return $ T.mkTclInt (B.length (T.asBStr s))
    _   -> argErr "string length"

string_compare args = case args of
    [s1,s2] -> case compare (T.asBStr s1) (T.asBStr s2) of
                  LT -> return (T.mkTclInt (-1))
                  GT -> return (T.mkTclInt 1)
                  EQ -> return (T.mkTclInt 0)
    _       -> argErr "string compare"

string_match args = case map T.asBStr args of
   [s1,s2]        -> domatch False s1 s2
   [nocase,s1,s2] -> if nocase == pack "-nocase" then domatch True s1 s2 else argErr "string"
   _              -> argErr "string match"
 where domatch nocase a b = return (T.fromBool (match nocase a b))

string_index args = case args of
                     [s,i] -> do let str = T.asBStr s
                                 ind <- toInd str i
                                 if ind >= (B.length str) || ind < 0 
                                  then ret 
                                  else treturn $ B.take 1 (B.drop ind str)
                     _   -> argErr "string index"
 where toInd s i = (T.asInt i) `orElse` tryEnd s i
       tryEnd s i = if i .== "end" 
                       then return ((B.length s) - 1) 
                       else do let (ip,is) = B.splitAt (length "end-") (T.asBStr i)
                               if ip == pack "end-"
                                  then case B.readInt is of
                                            Just (iv,_) -> return ((B.length s) - (1+iv))
                                            _           -> tclErr "bad index"
                                  else tclErr "bad index"

procAppend args = case args of
            (v:vx) -> do val <- varGet (T.asBStr v) `ifFails` T.empty
                         let cated = oconcat (val:vx)
                         varSet (T.asBStr v) cated
            _  -> argErr "append"
 where oconcat = T.mkTclBStr . B.concat . map T.asBStr

procSplit args = case args of
        [str]       -> dosplit (T.asBStr str)  (pack "\t\n ")
        [str,chars] -> let splitChars = T.asBStr chars 
                       in if B.null splitChars then return $ (T.mkTclList . map (T.mkTclBStr . B.singleton) . unpack) (T.asBStr str)
                                               else dosplit (T.asBStr str) splitChars
        _           -> argErr "split"

 where dosplit str chars = return $ T.mkTclList (map T.mkTclBStr (B.splitWith (\v -> v `B.elem` chars) str))

stringTests = TestList [ matchTests ]
matchTests = TestList [
    "boo" `matches` "boo" 
    ,"" `matches` "" 
    ,"1" `matches` "1" 
    ,"a?c" `matches` "abc" 
    ,"a?c" `doesnt_match` "ab" 
    ,"a??d" `matches` "abcd" 
    ,"f??d" `matches` "feed" 
    ,"b??n" `matches` "been"
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
