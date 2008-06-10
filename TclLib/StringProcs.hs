{-# LANGUAGE BangPatterns,OverloadedStrings #-}
module TclLib.StringProcs (stringProcs, stringTests) where

import Common
import Util
import Match (match, matchTests)
import qualified Data.ByteString.Char8 as B
import qualified TclObj as T
import Data.Char (toLower,toUpper)
import Control.Monad (when)
import TclLib.LibUtil

import Test.HUnit

stringProcs = makeCmdList [("string", cmdString), ("append", cmdAppend), ("split", cmdSplit)]

cmdString = mkEnsemble "string" [
   ("trim", string_op "trim" T.trim), 
   ("tolower", string_op "tolower" (B.map toLower)),
   ("toupper", string_op "toupper" (B.map toUpper)),
   ("reverse", string_op "reverse" B.reverse),
   ("length", string_length), ("range", string_range),
   ("match", string_match), ("compare", string_compare),
   ("equal", string_equal), ("index", string_index)
 ]

string_op name op args = case args of
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

string_equal args = case args of
  [s1,s2] -> eqcheck id s1 s2
  [opt,s1,s2] -> do 
     let optstr = T.asBStr opt
     when (optstr /= "-nocase") $ tclErr ("bad option " ++ show optstr)
     eqcheck downCase s1 s2
  _       -> argErr "string equal"
 where eqcheck f a b = return . T.fromBool $ f (T.asBStr a) == f (T.asBStr b)


string_match args = case map T.asBStr args of
   [s1,s2]        -> domatch False s1 s2
   [nocase,s1,s2] -> if nocase == "-nocase" then domatch True s1 s2 else argErr "string match"
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
       let slen = B.length (T.asBStr s)
       ind1 <- toIndex slen i1
       ind2 <- toIndex slen i2
       treturn $ B.drop ind1 (B.take (ind2+1) str)
   _ -> argErr "string range"

cmdAppend args = case args of
            (v:vx) -> do val <- varGetNS (T.asVarName v) `ifFails` T.empty
                         let cated = oconcat (val:vx)
                         varSetNS (T.asVarName v) cated
            _  -> argErr "append"
 where oconcat = T.fromBStr . B.concat . map T.asBStr

cmdSplit args = case args of
        [str]       -> dosplit (T.asBStr str) (pack "\t\n ")
        [str,chars] -> let splitChars = T.asBStr chars 
                       in case B.length splitChars of
                            0 -> lreturn $ map B.singleton (T.asStr str)
                            1 -> lreturn $! B.split (B.head splitChars) (T.asBStr str)
                            _ -> dosplit (T.asBStr str) splitChars
        _           -> argErr "split"
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
