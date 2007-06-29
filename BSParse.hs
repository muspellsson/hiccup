{-# OPTIONS_GHC -fbang-patterns #-}
module BSParse (parseArgs,runParse,getInterp,TclWord(..),dropWhite) where
import qualified Data.ByteString.Char8 as B
import Control.Monad
import Data.Char
import Test.HUnit  -- IGNORE

data TclWord = Word B.ByteString | Subcommand [TclWord] | NoSub B.ByteString deriving (Show,Eq)

dispatch str = do h <- safeHead str
                  case h of
                   '{' -> nested str 
                   '[' -> parseSub str
                   '"' -> parseStr str
                   _  -> getword str

parseArgs = multi (dispatch . dropWhite)
runParse = multi (mainparse . dropWhite)

safeHead s = guard (not (B.null s)) >> return (B.head s)


getInterp str = do 
   loc <- B.findIndex (\x -> x == '$' || x == '[') str
   let locval = B.index str loc
   if escaped loc str 
     then do (p,v,r) <- getInterp (B.drop (loc+1) str)
             return (B.append (B.take (loc-1) str) (B.cons locval p), v, r)
     else let (pre,aft) = B.splitAt loc str in
          case B.index str loc of
           '$' -> do (s, rest) <- getword aft
                     return (pre, s,rest)
           '[' -> do (s, rest) <- parseSub aft
                     return (pre, s, rest)

mainparse str = do h <- safeHead str
                   case h of 
                    ';'  -> return ([], B.tail str) 
                    '\n' -> return ([], B.tail str)
                    '#'  -> eatcomment str
                    _    -> parseArgs str

multi p s = do (w,r) <- p s
               case multi p r of
                Nothing -> return ([w],r)
                Just (wx,r2) -> return $! (w:wx,r2)

parseSub s = do guard (B.head s == '[') 
                (p,r) <- parseArgs (B.tail s)
                loc <- B.elemIndex ']' r
                let (pre,aft) = B.splitAt loc r
                return (Subcommand p, B.tail aft)

eatcomment = return . (,) [] . B.tail . B.dropWhile (/= '\n')

dropWhite = B.dropWhile (\x -> x == ' ' || x == '\t')

wordChar ' ' = False
wordChar c = let ci = ord c in
  (ord 'a' <= ci  && ci <= ord 'z') || (ord 'A' <= ci  && ci <= ord 'Z') || 
  (ord '0' <= ci  && ci <= ord '9') || (c `B.elem` (B.pack "+-*=/:^$%!^&<>"))

getword s = if B.null w then fail "can't parse word" else return (Word w,n)
 where (w,n) = B.span wordChar s

parseStr s = do loc <- B.elemIndex '"' str
                let (w,r) = B.splitAt loc str 
                if escaped loc str then do (Word w1, v) <- parseStr r
                                           let nw =  B.snoc (B.take (B.length w - 1) w) '"'
                                           return (ueword (B.append nw w1), v)
                                   else return (ueword w, B.tail r)
 where str = B.tail s
       ueword = Word . strSub (B.pack "\\t", B.singleton '\t') . strSub (B.pack "\\n",B.singleton '\n')

strSub (from,to) s = B.concat $ reverse $ breakUp s nls
 where nls = reverse $ B.findSubstrings from s
       breakUp x [] = [x]
       breakUp x (i:xs) = let (a,b) = B.splitAt i x in (B.drop (B.length from) b):to:breakUp a xs

escaped v s = escaped' v
 where escaped' !i = if (i <= 0) then False else (B.index s (i-1) == '\\') && not (escaped' (i-1))


nested s = do ind <- match 0 0
              let (w,r) = B.splitAt ind s
              return (NoSub (B.tail w), (B.tail r))
 where match !c !i 
        | B.length s <= i = fail $ "Couldn't match bracket" ++ show s
        | otherwise       = 
           case B.index s i of 
            '}' -> if c == 1 then return i else match (c-1) (i+1)
            '{' ->  match (c+1) (i+1)
            _   ->  match c (i+1) 

-- # TESTS # --

testNested = "Fail nested" ~: Nothing ~=? nested (bp "  {       the end")
testNested2 = "Pass nested" ~: Just (NoSub (bp "  { }"), B.empty) ~=? nested (bp "{  { }}")
testNested3 = "Fail nested" ~: Nothing ~=? nested (bp "  { {  }")

testEscaped = (escaped 1 (B.pack "\\\"")) ~? "pre-slashed quote should be escaped"
testEscaped2 = TestCase $ assertBool "non-slashed quote not escaped"  (not (escaped 1 (B.pack " \"")))
testEscaped3 = TestCase $ assertBool "pre-slashed quote should be escaped" (escaped 2 (B.pack " \\\""))
testEscaped4 = TestCase $ assertBool "non-slashed quote not escaped"  (not (escaped 2 (B.pack "  \"")))

bp = B.pack
mklit = Word . bp 
mkwd = Word . bp
testParseStr = "Escaped works" ~: Just (mklit "Oh \"yeah\" baby.", B.empty) ~=? parseStr (bp "\"Oh \\\"yeah\\\" baby.\"")
testParseStrLeft = "Parse Str with leftover" ~: Just (mklit "Hey there.", bp " 44") ~=? parseStr (bp "\"Hey there.\" 44")

testGetInterp =  "Escaped $ works"   ~: Nothing ~=? getInterp (bp "a \\$variable")
testGetInterp2 = "unescaped $ works" ~: Just (bp "a ", mkwd "$variable", bp "")  ~=? getInterp (bp "a $variable")
testGetInterp3 = "Escaped ["   ~: Nothing ~=? getInterp (bp "a \\[sub] thing.")
testGetInterp4 = "Escaped []"   ~: Nothing ~=? getInterp (bp "a \\[sub\\] thing.")
testGetInterp5 = "Escaped [] crazy"   ~: 
   Just (bp "a ",Subcommand [mkwd "sub",mklit "quail [puts 1]"], bp " thing.") ~=? getInterp (bp "a [sub \"quail [puts 1]\"] thing.")
testGetInterp6 = "unescaped $ works" ~: Just (bp "a $", mkwd "$variable", bp "")  ~=? getInterp (bp "a \\$$variable")


nestedTests= TestList [testNested, testNested2, testNested3]


getInterpTests = TestList [ testGetInterp, testGetInterp2, testGetInterp3, testGetInterp4, testGetInterp5, testGetInterp6 ]


tests = TestList [ nestedTests, testEscaped, testEscaped2, testEscaped3, testEscaped4, testParseStr, testParseStrLeft, 
                   getInterpTests ]

runUnit = runTestTT tests

-- # ENDTESTS # --
