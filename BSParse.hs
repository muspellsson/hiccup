{-# OPTIONS_GHC -fbang-patterns #-}

module BSParse ( runParse, wrapInterp, TclWord(..), dropWhite, parseList
            ,bsParseTests 
  ) where

import qualified Data.ByteString.Char8 as B
import Control.Monad
import Data.Ix
import Test.HUnit  -- IGNORE

type Result = Maybe ([[TclWord]], B.ByteString)

data TclWord = Word !B.ByteString | Subcommand [TclWord] | NoSub !B.ByteString Result deriving (Show,Eq)

dispatch str = do h <- safeHead str
                  case h of
                   '{' -> nested str 
                   '[' -> parseSub str
                   '"' -> parseStr str
                   _  -> getword str


listDisp str = do h <- safeHead str
                  case h of
                   '{' -> nested str >>= \((NoSub w _), r) -> return (w, r)
                   '"' -> parseStr str >>= \((Word w), r) -> return (w,r)
                   _   -> getListItem str

mkNoSub s = NoSub s (runParse s)

parseArgs = multi (dispatch . dropWhite)

parseList = multi (listDisp . dropWhite)

runParse :: B.ByteString -> Result
runParse = multi (mainparse . dropWhite)

safeHead s = guard (not (B.null s)) >> return (B.head s)

wrapInterp str = case getInterp str of
                   Nothing -> Left $! escapeStr str
                   Just (pr,s,r) -> Right  $! (escapeStr pr, s, r)

getInterp str = do 
   loc <- B.findIndex (\x -> x == '$' || x == '[') str
   let locval = B.index str loc
   if escaped loc str
     then dorestfrom loc locval
     else let (pre,aft) = B.splitAt loc str in
          let res = case locval of
                     '$' -> do (s, rest) <- (brackVar `orElse` getvar) (B.tail aft)
                               case getInd rest of
                                 Nothing    -> return (pre, Left s, rest)
                                 Just (i,r) -> return (pre, Left (B.append s i), r)
                     '[' -> do (Subcommand s, rest) <- parseSub aft
                               return (pre, Right s, rest)
                     _   -> fail "should've been $ or [ in getInterp"
          in res `mplus` dorestfrom loc locval
 where dorestfrom loc lval = do (p,v,r) <- getInterp (B.drop (loc+1) str)
                                return (B.append (B.take loc str) (B.cons lval p), v, r)

getInd str  
  | B.null str || B.head str /= '(' = Nothing
  | otherwise                       = do ind <- B.elemIndex ')' str
                                         let (pre,post) = B.splitAt (ind+1) str
                                         return (pre, post)
           
orElse a b = \v -> (a v) `mplus` (b v)

mainparse str = if B.null str 
                   then return ([], B.empty) 
                   else do
                       h <- safeHead str
                       case h of 
                        ';'  -> return ([], B.tail str) 
                        '\n' -> return ([], B.tail str)
                        '#'  -> eatcomment str
                        _    -> parseArgs str

multi p s = do (w,r) <- p s
               if B.null r 
                 then return ([w],r)
                 else case multi p r of
                       Nothing -> return ([w],r)
                       Just (wx,r2) -> return $! (w:wx,r2)
{-# INLINE multi #-}

parseSub s = do guard (B.head s == '[') 
                (p,r) <- parseArgs (B.tail s)
                loc <- B.elemIndex ']' r
                let (_,aft) = B.splitAt loc r -- TODO: Ignores unparsed.. tsk tsk.
                return (Subcommand p, B.tail aft)

eatcomment = return . (,) [] . B.drop 1 . B.dropWhile (/= '\n')

dropWhite = B.dropWhile (\x -> x == ' ' || x == '\t')

{-
wordChar ' ' = False
wordChar !c = let ci = ord c in
  (ord 'a' <= ci  && ci <= ord 'z') || (ord 'A' <= ci  && ci <= ord 'Z') || 
  (ord '0' <= ci  && ci <= ord '9') || (c == '_') -}
--wordChar !c = c /= ' ' && any (`inRange` c) [('a','z'),('A','Z'), ('0','9')]  || c == '_'
wordChar !c = c /= ' ' && (inRange ('a','z') c || inRange ('A','Z') c || inRange ('0','9') c || c == '_')

getword s = if B.null w then fail "can't parse word" else return (Word w,n)
 where (w,n) = B.span (\x -> wordChar x || (x `B.elem` (B.pack "$+.-*()=/:^%!&<>"))) s

getvar s = if B.null w then fail "can't parse var name" else return $! (w,n)
 where (w,n) = B.span wordChar s

getListItem s = if B.null w then fail "can't parse list item" else return (w,n)
 where (w,n) = B.span (`B.notElem` (B.pack " \t\n{}\"")) s

brackVar x = do hv <- safeHead x
                guard (hv == '{')
                let (b,a) = B.span (/='}') (B.tail x)
                safeHead a
                return $! (b, B.tail a)

parseStr s = do loc <- B.elemIndex '"' str
                let (w,r) = B.splitAt loc str 
                if escaped loc str then do (Word w1, v) <- parseStr r
                                           let nw =  B.snoc (B.take (B.length w - 1) w) '"'
                                           return (Word (B.append nw w1), v)
                                   else return (Word w, B.tail r)
 where str = B.tail s

escapeStr = optim
 where escape' !esc !lx = 
          if B.null lx then lx
                       else let (x,xs) = (B.head lx, B.tail lx)
                            in case (x, esc) of
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

escaped v s = escaped' v
 where escaped' !i = if (i <= 0) then False else (B.index s (i-1) == '\\') && not (escaped' (i-1))


nested s = do ind <- match 0 0 False
              let (w,r) = B.splitAt ind s
              return (mkNoSub (B.tail w), (B.tail r))
 where match !c !i !esc 
        | B.length s <= i = fail $ "Couldn't match bracket" ++ show s
        | otherwise       = 
           let nexti = i+1 in
           case B.index s i of 
            '}'  -> if esc then match c nexti False else (if c == 1 then return i else match (c-1) nexti False)
            '{'  -> if esc then match c nexti False else match (c+1) nexti False
            '\\' -> match c nexti (not esc)
            _    -> match c nexti False

 
-- # TESTS # --

testEscaped = TestList [
        (escaped 1 (B.pack "\\\"")) ~? "pre-slashed quote should be escaped",
        checkFalse "non-slashed quote not escaped"  (escaped 1 (B.pack " \"")),
        checkFalse "non-slashed quote not escaped"  (escaped 1 (B.pack " \"")),
        (escaped 2 (B.pack " \\\"")) ~? "pre-slashed quote should be escaped",
        checkFalse "non-slashed quote not escaped"  (escaped 2 (B.pack "  \""))
  ]
 where checkFalse str val = TestCase $ assertBool str (not val)

bp = B.pack
mklit = Word . bp 
mkwd = Word . bp

parseStrTests = TestList [
      "Escaped works" ~: (mkwd "Oh \"yeah\" baby.", B.empty) ?=? "\"Oh \\\"yeah\\\" baby.\"", 
      "Parse Str with leftover" ~: (mkwd "Hey there.", bp " 44") ?=? "\"Hey there.\" 44",
      "Parse Str with dolla" ~: (mklit "How about \\$44?", B.empty) ?=? "\"How about \\$44?\"",
      "bad parse1" ~: badParse "What's new?"
   ]
 where (?=?) res str = Just res ~=? parseStr (bp str)
       badParse str = Nothing ~=? parseStr (bp str)

brackVarTests = TestList [
      "Simple" ~: (bp "data", B.empty) ?=? "{data}",
      "With spaces" ~: (bp " a b c d ", bp " ") ?=? "{ a b c d } ",
      "bad parse" ~: badParse "{ oh no",
      "bad parse" ~: badParse "pancake"
   ]
 where (?=?) res str = Just res ~=? brackVar (bp str)
       badParse str = Nothing ~=? brackVar (bp str)

getInterpTests = TestList [
    "Escaped $ works" ~: noInterp "a \\$variable",
    "Bracket interp 1" ~: (bp "", mkvar "booga", bp "") ?=? "${booga}",
    "Bracket interp 2" ~: (bp "", mkvar "oh yeah!", bp "") ?=? "${oh yeah!}",
    "Bracket interp 3" ~: (bp " ", mkvar " !?! ", bp " ") ?=? " ${ !?! } ",
    "unescaped $ works" ~: 
          (bp "a ", mkvar "variable", bp "")  ?=? "a $variable",
    "escaped $ works" ~: 
          (bp "a \\$ ", mkvar "variable", bp "")  ?=? "a \\$ $variable",
    "escaped $ works 2" ~: 
          noInterp  "you deserve \\$44.",
    "adjacent interp works" ~: 
          (bp "", mkvar "var", bp "$bar$car")  ?=? "$var$bar$car",
    "interp after escaped dolla" ~: 
          (bp "a \\$", mkvar "name", bp " guy")  ?=? "a \\$$name guy",
    "interp after dolla" ~: 
          (bp "you have $", mkvar "dollars", bp "")  ?=? "you have $$dollars",
    "Escaped ["   ~: noInterp "a \\[sub] thing.",
    "Trailing bang" ~: (bp "", mkvar "var", bp "!" ) ?=? "$var!",
    "basic arr" ~: (bp "", mkvar "boo(4)", bp " " ) ?=? "$boo(4) ",
    "basic arr" ~: (bp "", mkvar "boo( 4,5 )", bp " " ) ?=? "$boo( 4,5 ) ",
    "Escaped []"   ~: noInterp "a \\[sub\\] thing.",
    "Lone $ works" ~: noInterp "a $ for the head of each rebel!",
    "Escaped lone $ works" ~: noInterp "a \\$ for the head of each rebel!",
    "unescaped $ after esc works" ~: 
          (bp "a \\$", mkvar "variable", bp "") ?=? "a \\$$variable",
    "Escaped [] crazy" ~:
       (bp "a ",Right [mkwd "sub",mklit "quail [puts 1]"], bp " thing.") ?=? "a [sub \"quail [puts 1]\"] thing."
  ]
 where noInterp str = Nothing ~=? getInterp (bp str)
       (?=?) res str = Just res ~=? getInterp (bp str)
       mkvar w = Left (B.pack w)

wrapInterpTests = TestList [
    "dollar escape"  ~: "oh $ yeah" ?!= "oh \\$ yeah",
    "brace escape"  ~: "oh [ yeah" ?!= "oh \\[ yeah",
    "tab escape"     ~: "a \t tab"  ?!= "a \\t tab",
    "slash escape"     ~: "slash \\\\ party"  ?!= "slash \\\\\\\\ party",
    "newline escape" ~: "\nline\n"  ?!= "\\nline\\n"
  ]
 where (?=?) res str = Right res ~=? wrapInterp (bp str)
       (?!=) res str = Left (bp res) ~=? wrapInterp (bp str)

getWordTests = TestList [
     "Simple" ~: badword "",
     "Simple2" ~: (mkwd "$whoa", bp "") ?=? "$whoa",
     "Simple with bang" ~: (mkwd "whoa!", bp " ") ?=? "whoa! "
  ]
 where badword str = Nothing ~=? getword (bp str)
       (?=?) res str = Just res ~=? getword(bp str)

nestedTests = TestList [
  "Fail nested" ~: Nothing ~=? nested (bp "  {       the end"),
  "Pass nested" ~: Just (mkNoSub (bp "  { }"), B.empty) ~=? nested (bp "{  { }}"),
  "Pass empty nested" ~: Just (mkNoSub (bp " "), B.empty) ~=? nested (bp "{ }"),
  "Fail nested" ~: Nothing ~=? nested (bp "  { {  }"),
  "Pass escape" ~: "{ \\{ }" `should_be` " \\{ ",
  "Pass escape 2" ~: "{ \\{ \\{ }" `should_be` " \\{ \\{ ",
  "Pass escape 3" ~: "{ \\\\}" `should_be` " \\\\",
  "Pass escape 4" ~: "{ \\} }" `should_be` " \\} "
 ]
 where should_be act exp = Just (mkNoSub (bp exp), B.empty) ~=? nested (bp act)

parseArgsTests = TestList [
     " x " ~: "x" ?=> ([mkwd "x"], "")  
     ," x y " ~: " x y " ?=> ([mkwd "x", mkwd "y"], " ")  
     ,"x y" ~: "x y" ?=> ([mkwd "x", mkwd "y"], "")  
     ,"x { y 0 }" ~: "x { y 0 }" ?=> ([mkwd "x", nosub " y 0 "], "")  
     ,"x {y 0}" ~: "x {y 0}" ?=> ([mkwd "x", nosub "y 0"], "")  
   ]
 where (?=>) str (res,r) = Just (res, bp r) ~=? parseArgs (bp str)
       nosub s = mkNoSub (bp s)

parseListTests = TestList [
     " x " ~: " x " ?=> ([bp "x"], " ")  
     ," x y " ~: " x y " ?=> ([bp "x", bp "y"], " ")  
     ,"x y" ~: "x y" ?=> ([bp "x", bp "y"], "")  
     ,"x { y 0 }" ~: "x { y 0 }" ?=> ([bp "x", bp " y 0 "], "")  
     ,"x [puts yay]" ~: "x [puts yay]" ?=> ([bp "x", bp "[puts", bp "yay]"], "")  
     ," y { \\{ \\{ \\{ } { x }" ~: " y { \\{ \\{ \\{ } { x }" ?=> ([bp "y", bp " \\{ \\{ \\{ ", bp " x "], "")  
     , "unmatched fail" ~: fails " { { "
     ,"x {y 0}" ~: "x {y 0}" ?=> ([bp "x", bp "y 0"], "")  
   ]
 where (?=>) str (res,r) = Just (res, bp r) ~=? parseList (bp str)
       fails str = Nothing ~=? parseList (bp str)

runParseTests = TestList [
     "one token" ~: ([[mkwd "exit"]],"") ?=? "exit",
     "empty" ~: ([[]],"") ?=? " ",
     "empty2" ~: ([[]],"") ?=? "",
--     "a b " ~: ([[mkwd "a", mkwd "b"]],"") ?=? "a b ",
     "arr 1" ~: ([[mkwd "set",mkwd "buggy(4)", mkwd "11"]], "") ?=? "set buggy(4) 11"
  ]
 where badword str = Nothing ~=? runParse (bp str)
       (?=?) (res,r) str = Just (res, bp r) ~=? runParse (bp str)

bsParseTests = TestList [ nestedTests, testEscaped, brackVarTests,
                   parseStrTests, getInterpTests, getWordTests, wrapInterpTests,
                   parseArgsTests, parseListTests, runParseTests ]

runUnit = runTestTT bsParseTests


-- # ENDTESTS # --
