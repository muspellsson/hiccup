{-# LANGUAGE BangPatterns,OverloadedStrings #-}

module TclParse ( TclWord(..)
                 ,doInterp 
                 ,runParse 
                 ,parseList 
                 ,tclParseTests
                )  where
 

import qualified Data.ByteString.Char8 as B
import BSParse ( TclWord(..), getInterp, runParse, parseList )
import Test.HUnit


doInterp str = case getInterp str of
                   Left _ -> Left (escapeStr str)
                   Right ((pr,s), r) -> Right (escapeStr pr, s, r)

escapeStr = optim
 where escape' !esc !lx =
          case B.uncons lx of
            Nothing -> lx
            Just (x,xs) -> case (x, esc) of
                             ('\\', False) -> escape' True xs
                             ('\\', True)  -> B.cons x (optim xs)
                             (_, False)    -> B.cons x (optim xs)
                             (_, True)     -> B.cons (escapeChar x) (optim xs)
       optim s = case B.elemIndex '\\' s of
                    Nothing -> s
                    Just i  -> let (c,r) = B.splitAt i s in B.append c (escape' True (B.drop 1 r))
       escapeChar 'n' = '\n'
       escapeChar 't' = '\t'
       escapeChar  c  = c


tclParseTests = TestList [ doInterpTests ]

doInterpTests = TestList [
    "dollar escape"  ~: "oh $ yeah" ?!= "oh \\$ yeah",
    "brace escape"  ~: "oh [ yeah" ?!= "oh \\[ yeah",
    "tab escape"     ~: "a \t tab"  ?!= "a \\t tab",
    "slash escape"     ~: "slash \\\\ party"  ?!= "slash \\\\\\\\ party",
    "subcom"   ~: ("some ", Right (Word "cmd", []), "") ?=? "some [cmd]",
    "newline escape" ~: "\nline\n"  ?!= "\\nline\\n"
  ]
 where (?=?) res str = Right res ~=? doInterp str
       (?!=) res str = Left res ~=? doInterp str
