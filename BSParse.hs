{-# OPTIONS_GHC -fbang-patterns #-}

module BSParse (parseArgs,runParse,wrapInterp,TclWord(..),dropWhite) where
import qualified Data.ByteString.Char8 as B
import Control.Monad
import Data.Char
import Test.HUnit  -- IGNORE

type Result = Maybe ([[TclWord]], B.ByteString)

data TclWord = Word !B.ByteString | Subcommand [TclWord] | NoSub !B.ByteString Result deriving (Show,Eq)

dispatch str = do h <- safeHead str
                  case h of
                   '{' -> nested str 
                   '[' -> parseSub str
                   '"' -> parseStr str
                   _  -> getword str

mkNoSub s = NoSub s (runParse s)

parseArgs = multi (dispatch . dropWhite)
runParse :: B.ByteString -> Result
runParse = multi (mainparse . dropWhite)

safeHead s = guard (not (B.null s)) >> return (B.head s)

wrapInterp str = case getInterp str of
                   Nothing -> Left $! escapeStr str
                   Just (pr,s,r) -> Right (escapeStr pr, s, r)

getInterp str = do 
   loc <- B.findIndex (\x -> x == '$' || x == '[') str
   let locval = B.index str loc
   if escaped loc str
     then dorestfrom loc locval
     else let (pre,aft) = B.splitAt loc str in
          let res = case locval of
                     '$' -> do (s, rest) <- (brackVar `orElse` getvar) (B.tail aft)
                               return (pre, s, rest)
                     '[' -> do (s, rest) <- parseSub aft
                               return (pre, s, rest)
          in res `mplus` dorestfrom loc locval
 where dorestfrom loc lval = do (p,v,r) <- getInterp (B.drop (loc+1) str)
                                return (B.append (B.take loc str) (B.cons lval p), v, r)

orElse a b = \v -> (a v) `mplus` (b v)

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
{-# INLINE multi #-}

parseSub s = do guard (B.head s == '[') 
                (p,r) <- parseArgs (B.tail s)
                loc <- B.elemIndex ']' r
                let (pre,aft) = B.splitAt loc r
                return (Subcommand p, B.tail aft)

eatcomment = return . (,) [] . B.tail . B.dropWhile (/= '\n')

dropWhite = B.dropWhile (\x -> x == ' ' || x == '\t')

wordChar ' ' = False
wordChar !c = let ci = ord c in
  (ord 'a' <= ci  && ci <= ord 'z') || (ord 'A' <= ci  && ci <= ord 'Z') || 
  (ord '0' <= ci  && ci <= ord '9') || (c == '_')

getword s = if B.null w then fail "can't parse word" else return (Word w,n)
 where (w,n) = B.span (\x -> wordChar x || (x `B.elem` (B.pack "$+.-*=/:^%!&<>"))) s

getvar s = if B.null w then fail "can't parse var name" else return (Word w,n)
 where (w,n) = B.span wordChar s

brackVar x = do hv <- safeHead x
                guard (hv == '{')
                let (b,a) = B.span (/='}') (B.tail x)
                safeHead a
                return (Word b,(B.tail a))

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
                                 ('\\', True) -> B.cons x (optim xs)
                                 (_,False)   -> B.cons x (optim xs)
                                 (_,True)    -> B.cons (escapeChar x) (optim xs)
       optim s = let (c,r) = B.span (/= '\\') s in B.append c (escape' False r)
       escapeChar 'n' = '\n'
       escapeChar 't' = '\t'
       escapeChar  c  = c

escaped v s = escaped' v
 where escaped' !i = if (i <= 0) then False else (B.index s (i-1) == '\\') && not (escaped' (i-1))


nested s = do ind <- match 0 0
              let (w,r) = B.splitAt ind s
              return (mkNoSub (B.tail w), (B.tail r))
 where match !c !i 
        | B.length s <= i = fail $ "Couldn't match bracket" ++ show s
        | otherwise       = 
           case B.index s i of 
            '}' -> if c == 1 then return i else match (c-1) (i+1)
            '{' ->  match (c+1) (i+1)
            _   ->  match c (i+1) 

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
      "Escaped works" ~: (mklit "Oh \"yeah\" baby.", B.empty) ?=? "\"Oh \\\"yeah\\\" baby.\"", 
      "Parse Str with leftover" ~: (mklit "Hey there.", bp " 44") ?=? "\"Hey there.\" 44",
      "Parse Str with dolla" ~: (mklit "How about \\$44?", B.empty) ?=? "\"How about \\$44?\"",
      "bad parse1" ~: badParse "What's new?"
   ]
 where (?=?) res str = Just res ~=? parseStr (bp str)
       badParse str = Nothing ~=? parseStr (bp str)

brackVarTests = TestList [
      "Simple" ~: (mklit "data", B.empty) ?=? "{data}",
      "With spaces" ~: (mklit " a b c d ", bp " ") ?=? "{ a b c d } ",
      "bad parse" ~: badParse "{ oh no",
      "bad parse" ~: badParse "pancake"
   ]
 where (?=?) res str = Just res ~=? brackVar (bp str)
       badParse str = Nothing ~=? brackVar (bp str)

getInterpTests = TestList [
    "Escaped $ works" ~: noInterp "a \\$variable",
    "Bracket interp 1" ~: (bp "", mkwd "booga", bp "") ?=? "${booga}",
    "Bracket interp 2" ~: (bp "", mkwd "oh yeah!", bp "") ?=? "${oh yeah!}",
    "Bracket interp 3" ~: (bp " ", mkwd " !?! ", bp " ") ?=? " ${ !?! } ",
    "unescaped $ works" ~: 
          (bp "a ", mkwd "variable", bp "")  ?=? "a $variable",
    "escaped $ works" ~: 
          (bp "a \\$ ", mkwd "variable", bp "")  ?=? "a \\$ $variable",
    "escaped $ works 2" ~: 
          noInterp  "you deserve \\$44.",
    "adjacent interp works" ~: 
          (bp "", mkwd "var", bp "$bar$car")  ?=? "$var$bar$car",
    "interp after escaped dolla" ~: 
          (bp "a \\$", mkwd "name", bp " guy")  ?=? "a \\$$name guy",
    "interp after dolla" ~: 
          (bp "you have $", mkwd "dollars", bp "")  ?=? "you have $$dollars",
    "Escaped ["   ~: noInterp "a \\[sub] thing.",
    "Trailing bang" ~: (bp "", mkwd "var", bp "!" ) ?=? "$var!",
    "Escaped []"   ~: noInterp "a \\[sub\\] thing.",
    "Lone $ works" ~: noInterp "a $ for the head of each rebel!",
    "Escaped lone $ works" ~: noInterp "a \\$ for the head of each rebel!",
    "unescaped $ after esc works" ~: 
          (bp "a \\$", mkwd "variable", bp "") ?=? "a \\$$variable",
    "Escaped [] crazy" ~:
       (bp "a ",Subcommand [mkwd "sub",mklit "quail [puts 1]"], bp " thing.") ?=? "a [sub \"quail [puts 1]\"] thing."
  ]
 where noInterp str = Nothing ~=? getInterp (bp str)
       (?=?) res str = Just res ~=? getInterp (bp str)

wrapInterpTests = TestList [
    "simple escape" ~: "oh $ yeah" ?!= "oh \\$ yeah"
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
  "Fail nested" ~: Nothing ~=? nested (bp "  { {  }")
 ]

tests = TestList [ nestedTests, testEscaped, brackVarTests,
                   parseStrTests, getInterpTests, getWordTests, wrapInterpTests ]

runUnit = runTestTT tests


-- # ENDTESTS # --
