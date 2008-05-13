{-# LANGUAGE BangPatterns,OverloadedStrings #-}
module TclLib.StringProcs (stringProcs, stringTests) where

import Common
import Util
import Match (match, matchTests)
import qualified Data.ByteString.Char8 as B
import qualified TclObj as T
import TclObj ((.==))
import Data.Char (toLower,toUpper)

import Test.HUnit

stringProcs = makeCmdList [("string", procString), ("append", procAppend), ("split", procSplit)]

procString = makeEnsemble "string" [
   ("trim", string_Op "trim" T.trim), 
   ("tolower", string_Op "tolower" (B.map toLower)),
   ("toupper", string_Op "toupper" (B.map toUpper)),
   ("reverse", string_Op "reverse" B.reverse),
   ("length", string_length), ("range", string_range),
   ("match", string_match), ("compare", string_compare),
   ("index", string_index)
 ]

string_Op name op args = case args of
   [s] -> treturn $! op (T.asBStr s)
   _   -> argErr $ "string " ++ name

string_length args = case args of
    [s] -> return $ T.fromInt (B.length (T.asBStr s))
    _   -> argErr "string length"

string_compare args = case map T.asBStr args of
    [s1,s2] -> return (ord2int (compare s1 s2))
    ["-nocase",s1,s2] -> return (ord2int (compare (downCase s1) (downCase s2)))
    _       -> argErr "string compare"
 where ord2int o = case o of
            LT -> T.fromInt (-1)
            GT -> T.fromInt 1
            EQ -> T.fromInt 0

string_match args = case map T.asBStr args of
   [s1,s2]        -> domatch False s1 s2
   [nocase,s1,s2] -> if nocase == pack "-nocase" then domatch True s1 s2 else argErr "string"
   _              -> argErr "string match"
 where domatch nocase a b = return (T.fromBool (match nocase a b))

string_index args = case args of
                     [s,i] -> do let str = T.asBStr s
                                 let slen = B.length str
                                 ind <- toInd slen i
                                 if ind >= slen || ind < 0 
                                  then ret 
                                  else treturn $ B.take 1 (B.drop ind str)
                     _   -> argErr "string index"

toInd :: (Monad m) => Int -> T.TclObj -> m Int
toInd len i = case T.asInt i of
                Nothing -> tryEnd
                Just iv -> return iv
 where ibs = T.asBStr i 
       lastInd = len - 1
       badIndex = fail "bad index"
       tryEnd = if ibs `B.isPrefixOf` "end" 
                  then return lastInd
                  else do let (ip,is) = B.splitAt (B.length "end-") ibs
                          if ip == "end-"
                              then case B.readInt is of
                                       Just (iv,_) -> return (lastInd - iv)
                                       _           -> badIndex
                              else badIndex

string_range args = case args of
   [s,i1,i2] -> do 
       let str = T.asBStr s
       let slen = B.length (T.asBStr s)
       ind1 <- toInd slen i1
       ind2 <- toInd slen i2
       treturn $ B.drop ind1 (B.take (ind2+1) str)
   _ -> argErr "string range"

procAppend args = case args of
            (v:vx) -> do val <- varGetNS (T.asVarName v) `ifFails` T.empty
                         let cated = oconcat (val:vx)
                         varSetNS (T.asVarName v) cated
            _  -> argErr "append"
 where oconcat = T.fromBStr . B.concat . map T.asBStr

procSplit args = case args of
        [str]       -> dosplit (T.asBStr str)  (pack "\t\n ")
        [str,chars] -> let splitChars = T.asBStr chars 
                       in if B.null splitChars then return $ (T.mkTclList . map (T.fromBStr . B.singleton) . unpack) (T.asBStr str)
                                               else dosplit (T.asBStr str) splitChars
        _           -> argErr "split"

 where dosplit str chars = return $ T.mkTclList (map T.fromBStr (B.splitWith (\v -> v `B.elem` chars) str))

stringTests = TestList [ matchTests, toIndTests ]


toIndTests = TestList [
     (someLen, T.fromStr "10") `should_be` 10
     ,(someLen, T.fromStr "end") `should_be` lastInd
     ,(someLen, T.fromStr "e") `should_be` lastInd
     ,(someLen, T.fromStr "en") `should_be` lastInd
     ,(someLen, T.fromStr "end-1") `should_be` (lastInd - 1)
     ,(someLen, T.fromStr "e-4") `should_fail` ()
  ] where should_be (l,i) b =  ((toInd l i) :: Either String Int)  ~=? (Right b)
          should_fail (l,i) _ =  ((toInd l i) :: Either String Int)  ~=? (Left "bad index")
          someLen = 5
          lastInd = someLen - 1
