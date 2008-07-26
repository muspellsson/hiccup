{-# LANGUAGE BangPatterns,OverloadedStrings #-}
module TclLib.StringProcs (stringProcs, stringTests) where

import Common
import Util
import Match (match, matchTests)
import qualified Data.ByteString.Char8 as B
import qualified TclObj as T
import Data.Char (toLower,toUpper,isSpace)
import TclLib.LibUtil
import ArgParse

import Test.HUnit

stringProcs = makeCmdList [("string", cmdString), ("append", cmdAppend), ("split", cmdSplit)]

cmdString = mkEnsemble "string" [
   ("trimleft", string_trimleft),
   ("trimright", string_trimright),
   ("trim", string_trim),
   ("tolower", string_op "tolower" (B.map toLower)),
   ("toupper", string_op "toupper" (B.map toUpper)),
   ("reverse", string_op "reverse" B.reverse),
   ("length", string_length), ("range", string_range),
   ("match", string_match), ("compare", string_compare),
   ("equal", string_equal), ("index", string_index)
 ]


string_trimleft = trimcmd "trimleft" trimleft
string_trimright = trimcmd "trimright" trimright
string_trim = trimcmd "trim" trim
trimleft pred = B.dropWhile pred
trimright pred s = fst (B.spanEnd pred s)
trim pred = trimright pred . trimleft pred

trimcmd name f args = case args of
  [s] -> go isSpace s
  [s,cs] -> go (elemOf cs) s
  _  -> vArgErr . pack $ "string " ++ name ++ " ?chars?"
 where go pred = treturn . f pred . T.asBStr
       elemOf s = let bstr = T.asBStr s in (`B.elem` bstr)
  

string_op name op args = case args of
   [s] -> treturn . op . T.asBStr $ s
   _   -> argErr $ "string " ++ name

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
     [s1,s2] -> specCompare cspec s1 s2 >>= return . ord2int 
     _       -> argErr "string compare"
 where ord2int o = case o of
            EQ -> T.fromInt 0
            LT -> T.fromInt (-1)
            GT -> T.fromInt 1


string_equal args_ = do
  (cspec, args) <- parseArgs compSpecs (CompSpec False Nothing) args_
  case args of
    [s1,s2] -> specCompare cspec s1 s2 >>= return . T.fromBool . (== EQ)
    _       -> argErr "string equal"

matchNoCase = boolFlagSpec "nocase" 2
string_match args_ = do
   (nocase,args) <- parseArgs matchNoCase False args_
   case args of
       [s1,s2]        -> domatch nocase (T.asBStr s1) (T.asBStr s2)
       _              -> argErr "string match"
 where domatch nocase a b = return (T.fromBool (match nocase a b))

string_index args = case args of
                     [s,i] -> do let str = T.asBStr s
                                 let slen = B.length str
                                 ind <- toIndex slen i
                                 if ind >= slen || ind < 0 
                                  then ret 
                                  else treturn $ B.take 1 (B.drop ind str)
                     _   -> argErr "string index"


string_range args = case args of
   [s,i1,i2] -> do 
       let str = T.asBStr s
       let slen = B.length str
       ind1 <- toIndex slen i1
       ind2 <- toIndex slen i2
       treturn $ B.drop ind1 (B.take (ind2+1) str)
   _ -> argErr "string range"

cmdAppend args = case args of
            (v:vx) -> do val <- varGetNS (T.asVarName v) `ifFails` T.empty
                         let cated = oconcat (val:vx)
                         varSetNS (T.asVarName v) cated
            _  -> vArgErr "append varName ?value value ...?"
 where oconcat = T.fromBStr . B.concat . map T.asBStr

cmdSplit args = case args of
        [str]       -> dosplit (T.asBStr str) (pack "\t\n ")
        [str,chars] -> let splitChars = T.asBStr chars 
                           bstr = T.asBStr str
                       in case B.length splitChars of
                            0 -> lreturn $ map B.singleton (B.unpack bstr)
                            1 -> lreturn $! B.split (B.head splitChars) bstr
                            _ -> dosplit bstr splitChars
        _           -> vArgErr "split string ?splitChars?"
 where dosplit str chars = lreturn (B.splitWith (\v -> v `B.elem` chars) str)
       lreturn l = return $! T.fromList . map T.fromBStr $ l

stringTests = TestList [ matchTests, toIndTests ]

toIndTests = TestList [
     (someLen, "10") `should_be` 10
     ,(someLen, "end") `should_be` lastInd
     ,(someLen, "e") `should_be` lastInd
     ,(someLen, "en") `should_be` lastInd
     ,(someLen, "end-1") `should_be` (lastInd - 1)
     ,(someLen, "e-4") `should_fail` ()
     ,(someLen, "") `should_fail` ()
  ] where should_be p b = show p ~: go p  ~=? (Right b)
          should_fail p _ =  show p ~: go p  ~=? (Left "bad index")
          go (l,i) = (toIndex l (T.fromStr i)) :: Either String Int
          someLen = 5
          lastInd = someLen - 1
