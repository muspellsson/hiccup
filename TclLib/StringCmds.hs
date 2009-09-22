{-# LANGUAGE BangPatterns,OverloadedStrings, FlexibleContexts #-}
module TclLib.StringCmds (stringCmds, 
                          stringInits,
                          stringTests) where

import Common
import Control.Monad (liftM)
import Util
import Match (match, matchTests)
import qualified System.IO.Error as IOE 
import qualified Data.ByteString.Char8 as B
import qualified TclObj as T
import Data.Char (toLower,toUpper,isSpace)
import Data.Maybe (listToMaybe)
import TclLib.LibUtil
import Text.Regex.Posix 
import ArgParse

import Test.HUnit

stringInits = [registerEnsem "string" cmdString]
stringCmds = makeCmdList [
    ("regexp", cmdRegexp),
    ("append", cmdAppend), ("split", cmdSplit)
  ]

cmdString = mkEnsemble "string" [
   ("trimleft", string_trimleft),
   ("trimright", string_trimright),
   ("trim", string_trim),
   ("first", string_first),
   ("map", string_map),
   ("tolower", string_op "tolower" (B.map toLower)),
   ("toupper", string_op "toupper" (B.map toUpper)),
   ("reverse", string_reverse),
   ("length", string_length), ("range", string_range),
   ("match", string_match), ("compare", string_compare),
   ("equal", string_equal), ("index", string_index)
 ]


string_trimleft = trimcmd "trimleft" trimleft
string_trimright = trimcmd "trimright" trimright
string_trim = trimcmd "trim" trim
trimleft pred = B.dropWhile pred
trimright pred = fst . B.spanEnd pred
trim pred = trimright pred . trimleft pred

trimcmd name f args = case args of
  [s] -> go isSpace s
  [s,cs] -> go (elemOf cs) s
  _  -> vArgErr . pack $ "string " ++ name ++ " ?chars?"
 where go pred = treturn . f pred . T.asBStr
       elemOf s = let bstr = T.asBStr s in (`B.elem` bstr)
  
string_reverse args = case args of
   [s] -> treturn $! (B.reverse (T.asBStr s))
   _   -> vArgErr "string reverse string"

string_op :: String -> (BString -> BString) -> [T.TclObj] -> TclM T.TclObj
string_op name op args = case args of
   [s] -> treturn . op . T.asBStr $ s
   [s,i1] -> do
      let bs = T.asBStr s
      ind1 <- toIndex (B.length bs) i1
      do_op bs ind1 1
   [s,i1,i2] -> do
      let bs = T.asBStr s
      [ind1,ind2] <- mapM (toIndex (B.length bs)) [i1,i2]
      let oplen = 1 + ind2 - ind1
      do_op bs ind1 oplen
   _   -> vArgErr . pack $ "string " ++ name ++ " string ?first? ?last?"
 where do_op str ind oplen = do
          let (pre,rest) = B.splitAt ind str
          let (mid,end) = B.splitAt oplen rest
          treturn . B.concat $ [pre, op mid, end]

string_length args = case args of
    [s] -> return . T.fromInt . B.length . T.asBStr $ s
    _   -> vArgErr "string length string"

data CompSpec = CompSpec { csNoCase :: Bool, csLen :: Maybe T.TclObj }


compSpecs = mkArgSpecs 2 [
     NoArg "nocase" (\cs -> cs { csNoCase = True }),
     OneArg "length" (\i cs -> cs { csLen = Just i })
 ]

specCompare (CompSpec nocase len) s1 s2 = do
  let modder1 = if nocase then downCase else id
  modder2 <- lengthMod
  let modder = modder1 . modder2 . T.asBStr
  return (compare (modder s1) (modder s2))
 where lengthMod = case len of
                    Nothing -> return id
                    Just o  -> do
                       i <- T.asInt o
                       if i < 0 then return id
                                else return (B.take i)

string_compare args_ = do
   (cspec, args) <- parseArgs compSpecs (CompSpec False Nothing) args_
   case args of
     [s1,s2] -> liftM ord2int (specCompare cspec s1 s2)
     _       -> vArgErr "string compare ?-nocase? ?-length int? string1 string2"
 where ord2int o = case o of
            EQ -> T.fromInt 0
            LT -> T.fromInt (-1)
            GT -> T.fromInt 1


string_equal args_ = do
  (cspec, args) <- parseArgs compSpecs (CompSpec False Nothing) args_
  case args of
    [s1,s2] -> specCompare cspec s1 s2 >>= return . T.fromBool . (== EQ)
    _       -> vArgErr "string equal ?-nocase? ?-length int? string1 string2"

noCaseSpec = boolFlagSpec "nocase" 2
string_match args_ = do
   (nocase,args) <- parseArgs noCaseSpec False args_
   case args of
       [s1,s2]        -> domatch nocase (T.asBStr s1) (T.asBStr s2)
       _              -> vArgErr "string match ?-nocase? pattern string"
 where domatch nocase a b = return (T.fromBool (Match.match nocase a b))

string_index args = case args of
                     [s,i] -> do let str = T.asBStr s
                                 let slen = B.length str
                                 ind <- toIndex slen i
                                 if ind >= slen || ind < 0 
                                  then ret 
                                  else treturn $ B.take 1 (B.drop ind str)
                     _   -> vArgErr "string index string charIndex"


string_map args_ = do
  (nocase,args) <- parseArgs noCaseSpec False args_
  case args of
   [tcm,s] -> do
    cm <- T.asList tcm >>= toPairs . map T.asBStr
    return . T.fromBStr . mapReplace nocase cm . T.asBStr $ s
   _     -> vArgErr "string map ?-nocase? charMap string"
 
-- TODO: This is inefficiently implemented, but can be easily improved.
mapReplace nocase ml = go
  where firstMatch str = let ncstr = downcase str
                         in listToMaybe [(k,v) | (k,v) <- ncml, k `B.isPrefixOf` ncstr]
        downcase = B.map toLower
        ncml = if nocase then mapFst downcase ml else ml
        go s = case firstMatch s of
                    Nothing -> case B.uncons s of
                                 Nothing -> s
                                 Just (c,r) -> B.cons c (go r)
                    Just (k,v) -> let rest = B.drop (B.length k) s
                                  in B.append v (go rest)

string_first args = case args of
  [s1,s2] -> let (bs1,bs2) = (T.asBStr s1, T.asBStr s2)
             in go bs1 bs2 0
  [s1,s2,ind] -> do 
     let (bs1,bs2) = (T.asBStr s1, T.asBStr s2)
     index <- toIndex (B.length bs2) ind
     go bs1 bs2 index
  _  -> vArgErr "string first needleString haystackString ?startIndex?"
 where go s1 s2 off = return . T.fromInt $ 
              case findSubstring s1 (B.drop off s2) of
                      Nothing -> -1
                      Just i  -> off + i

-- Copied from BS docs, since findSubstring is deprecated.
findSubstring :: B.ByteString -> B.ByteString -> Maybe Int
findSubstring s l 
  | B.null s = Just 0
  | otherwise = case B.breakSubstring s l of
                       (x,y) | B.null y  -> Nothing
                             | otherwise -> Just (B.length x)

string_range args = case args of
   [s,i1,i2] -> do 
       let str = T.asBStr s
       let slen = B.length str
       ind1 <- toIndex slen i1
       ind2 <- toIndex slen i2
       treturn $ B.drop ind1 (B.take (ind2+1) str)
   _ -> vArgErr "string range string first last"

cmdAppend args = case args of
            (v:vx) -> do val <- varGetNS (T.asVarName v) `ifFails` T.empty
                         let cated = oconcat (val:vx)
                         varSetNS (T.asVarName v) cated
            _  -> vArgErr "append varName ?value value ...?"
 where oconcat = T.fromBStr . B.concat . map T.asBStr

cmdSplit args = case args of
        [str]       -> dosplit (T.asBStr str) "\t\n "
        [str,chars] -> let splitChars = T.asBStr chars 
                           bstr = T.asBStr str
                       in case B.length splitChars of
                            0 -> lreturn $ map B.singleton (B.unpack bstr)
                            1 -> lreturn $! B.split (B.head splitChars) bstr
                            _ -> dosplit bstr splitChars
        _           -> vArgErr "split string ?splitChars?"
 where dosplit str chars = lreturn (B.splitWith (`B.elem` chars) str)

cmdRegexp :: [T.TclObj] -> TclM T.TclObj
cmdRegexp args = case args of
  [pat,str] -> liftM T.fromBool (wrapRE (T.asBStr str =~ T.asBStr pat))
  [pat,str,matchVar] -> do m <- wrapRE (T.asBStr str =~~ T.asBStr pat)
                           case m of
                              [] -> return (T.fromBool False)
                              (mv:_) -> do
                               varSetNS (T.asVarName matchVar) (T.fromBStr mv)
                               return (T.fromBool True)
  _         -> vArgErr "regexp exp string ?matchVar?"
 where wrapRE f = do r <- io $ IOE.try (return $! f)
                     case r of
                        Left _ -> fail "invalid regex"
                        Right v -> return $! v

stringTests = TestList [ matchTests, toIndTests, mapReplaceTests ]

mapReplaceTests = TestList [
   ([("a","1"),("b","2")], "aabbca", False) `should_be` "1122c1"
   ,([("a","1"),("b","2")], "bca", False) `should_be` "2c1"
   ,([("a","1"),("b","2")], "BCA", True) `should_be` "2C1"
   ,([("A","1"),("B","2")], "bca", True) `should_be` "2c1"
  ] where should_be (ml,s,nocase) b = b ~=?  mapReplace nocase ml s

toIndTests = TestList [
     (someLen, "10") `should_be` 10
     ,(someLen, "end") `should_be` lastInd
     ,(someLen, "e") `should_be` lastInd
     ,(someLen, "en") `should_be` lastInd
     ,(someLen, "end-1") `should_be` (lastInd - 1)
     ,(someLen, "e-4") `should_fail` ()
     ,(someLen, "") `should_fail` ()
  ] where should_be p b = show p ~: go p  ~=? (Right b)
          should_fail p@(_,s) _ =  show p ~: Left ("bad index: " ++ show s) ~=? go p
          go (l,i) = (toIndex l (T.fromStr i)) :: Either String Int
          someLen = 5
          lastInd = someLen - 1
