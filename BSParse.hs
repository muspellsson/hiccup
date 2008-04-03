{-# LANGUAGE BangPatterns,OverloadedStrings #-}
module BSParse ( runParse, doInterp, TclWord(..), parseList
            ,Result
            ,TokCmd
            ,bsParseTests
  ) where

import qualified Data.ByteString.Char8 as B
import Control.Monad
import Data.Ix
import Util hiding (orElse,escapeStr)
import Test.HUnit 

data TclWord = Word !B.ByteString 
             | Subcommand TokCmd 
             | NoSub !B.ByteString Result  
             | Expand TclWord deriving (Show,Eq)

type Parser a = BString -> PResult a
type ParseMonad a = Maybe a
type PResult a = ParseMonad (a, BString)
type Result = PResult [TokCmd]
type TokCmd = (TclWord, [TclWord])

runParse :: Parser [TokCmd]
runParse = parseStatements `wrapWith` asCmds
 where asCmds lst = [ let (c:a) = x in (c,a) | x <- lst, not (null x)]


parseStatements :: Parser [[TclWord]]
parseStatements = (multi (eatSpaces .>> parseStatement)) `pass` parseEof
  
-- TODO: Eep. Dirty
-- Pass parsers the second argument, but ignores the result.
pass :: Parser t -> Parser t1 -> Parser t
pass f1 f = \s -> f1 s >>= \(v,r) -> f r >>= \(_,r2) -> return $! (v,r2)
{-# INLINE pass #-}

parseStatement :: Parser [TclWord]
parseStatement = eatAndNext `orElse` parseTokens `orElse` parseEof
 where stmtSep = parseOneOf "\n;" .>> eatSpaces 
       eatAndNext = (stmtSep `orElse` parseComment) .>> parseStatement

(.>>) f1 f2 = \s -> do
        (_, r) <- f1 s
        f2 r
{-# INLINE (.>>) #-}

eatSpaces s = return ((), dropSpaces s)
{-# INLINE eatSpaces #-}
parseComment :: Parser ()
parseComment = parseChar '#' .>> getPred (/= '\n') .>> eatSpaces

parseTokens :: Parser [TclWord]
parseTokens = parseMany1 (eatSpaces .>> parseToken)

parseToken :: Parser TclWord
parseToken str = do 
   h <- safeHead str
   case h of
     '{'  -> (parseExpand `orElse` parseNoSub) str
     '['  -> (parseSub `wrapWith` Subcommand) str
     '"'  -> parseStr str
     '\\' -> handleEsc str
     _    -> wordToken str

handleEsc :: Parser TclWord
handleEsc str = do 
  s <- eatChar '\\' str 
  h <- safeHead s
  let rest = B.drop 1 s
  case h of
     '\n' -> (eatSpaces .>> parseToken) rest
     v    -> case wordTokenRaw rest of
                Just (w, r) -> return (Word (B.concat ["\\", B.singleton v, w]), r)
                Nothing     -> return (Word (B.cons '\\' (B.singleton v)), rest)

parseList s = if onlyWhite s
               then return []
               else do (l,r) <- parseMany (listDisp . dropWhite) $ s
                       guard (onlyWhite r)
                       return l
 where onlyWhite = B.all isWhite
       isWhite = (`elem` " \t\n")
       dropWhite = B.dropWhile isWhite

listDisp str = do h <- safeHead str
                  case h of
                   '{' -> nested str
                   '"' -> parseStrRaw str 
                   _   -> getListItem str

getListItem s = if B.null w then fail "can't parse list item" else return (w,n)
 where (w,n) = B.splitAt (listItemEnd s) s

listItemEnd s = inner 0 False where 
   inner i esc = if i == B.length s then i
                     else if esc then inner (i+1) False
                           else case B.index s i of
                                  '\\' -> inner (i+1) True
                                  v  -> if v `B.elem` "{}\" \t\n" then i else inner (i+1) False


safeHead s = guard (not (B.null s)) >> return (B.head s)
{-# INLINE safeHead #-}

doInterp str = case getInterp str of
                   Nothing -> Left (escapeStr str)
                   Just (pr,s,r) -> Right (escapeStr pr, s, r)

(.>-) f w s = wrapWith f w s

-- TODO: UGLY
getInterp str = do
   loc <- B.findIndex (\x -> x == '$' || x == '[') str
   let locval = B.index str loc
   if escaped loc str
     then dorestfrom loc locval
     else let (pre,aft) = B.splitAt loc str in
          let pfun = (doVarParse .>-  Left) `orElse` (parseSub .>- Right) in
          let res = pfun aft >>= \(v,rest) -> return (pre, v, rest) 
          in res `mplus` dorestfrom loc locval
 where dorestfrom loc lval = do (p,v,r) <- getInterp (B.drop (loc+1) str)
                                return (B.append (B.take loc str) (B.cons lval p), v, r)

doVarParse :: Parser BString
doVarParse = parseChar '$' .>> parseVarRef

parseVarRef :: Parser BString
parseVarRef s = do 
         let flist = [ parseVarTerm `orElse` getNS
                      ,tryGet parseVarRef
                      ,tryGet parseInd]
         chain flist s

getNS = chain [parseLit "::", parseVarTerm, tryGet getNS]

parseVarTerm :: Parser BString
parseVarTerm = getVar `orElse` brackVar
 where getVar = getPred1 wordChar

parseInd :: Parser BString
parseInd str = do 
    parseChar '(' str
    ind <- B.elemIndex ')' str
    let (pre,post) = B.splitAt (ind+1) str
    return (pre, post)



orElse :: Parser t -> Parser t -> Parser t
orElse a b = \v -> v `seq` ((a v) `mplus` (b v))
{-# INLINE orElse #-}

     
chain :: [Parser BString] -> Parser BString
chain lst !rs = inner lst [] rs
 where inner []     !acc !r = return (B.concat (reverse acc), r)
       inner (f:fs) !acc !r = do (s,r2) <- f r 
                                 inner fs (s:acc) r2
 
parseLit :: BString -> Parser BString
parseLit !w s = if w `B.isPrefixOf` s 
                    then return (w, B.drop (B.length w) s) 
                    else fail $ "didn't match " ++ show w

eatChar :: Char -> BString -> ParseMonad BString
eatChar c s = parseChar c s >>= return . snd
{-# INLINE eatChar #-}


{-
 -- Toys for later
parseInt :: Parser Int 
parseInt s = B.readInt s 

parseDouble :: Parser Double
parseDouble s = do
    (ds, r) <- chain [parseNums, parseChar '.', parseNums] s
    return (read (B.unpack ds), r)
 where parseNums = getPred1 (inRange ('0','9'))

parseParen :: Parser t -> Parser t
parseParen p s = do
  (parseChar '(' .>> eatSpaces .>> p) s >>= pass (eatSpaces .>> parseChar ')')
-}
         
parseOneOf !cl = parseCharPred (`elem` cl) ("one of " ++ show cl)
parseChar :: Char -> Parser BString
parseChar !c = parseCharPred (== c) (show c)

parseCharPred pred exp s = case B.uncons s of
                            Nothing    -> failStr "empty string"
                            Just (h,t) -> if pred h then return (B.singleton h,t)
                                                    else failStr (show h)
 where failStr what = fail $ "didn't match, expected " ++ exp ++ ", got " ++ what
{-# INLINE parseCharPred #-}

parseEof s = if B.null s then return ([], s) else fail "expected eof"

parseMany :: Parser t -> Parser [t]
parseMany p = inner `orElse` (\s -> return ([],s))
 where inner = parseMany1 p
   
parseMany1 p = pjoin (:) p (parseMany p)

pjoin :: (t1 -> t2 -> t3) -> Parser t1 -> Parser t2 -> Parser t3
pjoin op a b = \s -> do
                 (w,r)   <- a s
                 (w2,r2) <- b r
                 return ((op w w2), r2)
{-# INLINE pjoin #-}

multi :: Parser t -> Parser [t]
multi p = pjoin (:) p (parseEof `orElse` (multi p))
{-# INLINE multi #-}

brackets p = (parseChar '[' .>> p) `pass` (eatSpaces .>> parseChar ']')

parseSub :: Parser TokCmd
parseSub s = do 
  (p,aft) <- brackets parseTokens $ s
  case p of
    [] -> fail "empty subcommand"
    (ph:pt) -> return ((ph,pt), aft)


{-
wordChar ' ' = False
wordChar !c = let ci = ord c in
  (ord 'a' <= ci  && ci <= ord 'z') || (ord 'A' <= ci  && ci <= ord 'Z') ||
  (ord '0' <= ci  && ci <= ord '9') || (c == '_')
wordChar !c = c /= ' ' && any (`inRange` c) [('a','z'),('A','Z'), ('0','9')]  || c == '_'
-}
wordChar !c = c /= ' ' && (inRange ('a','z') c || inRange ('A','Z') c || inRange ('0','9') c || c == '_')

getPred p s = return $! (w,n)
 where (w,n) = B.span p s

getPred1 p s = if B.null w then fail "no match" else return $! (w,n)
 where (w,n) = B.span p s

getWord = getPred1 p
 where p c = wordChar c || (c `B.elem` "+.-=<>*()$/,:^%!&#|?")


tryGet fn s = (fn `orElse` (\_ -> return (B.empty, s))) s

wrapWith fn wr s = fn s >>= \(!w,r) -> return (wr w, r) 
{-# INLINE wrapWith #-}

wordToken = wordTokenRaw `wrapWith` Word
wordTokenRaw  = (chain [parseChar '$', parseVarBody]) `orElse` getWord

parseVarBody = (brackVar `wrapWith` brackIt) `orElse` (chain [getWord, tryGet wordTokenRaw])
 where brackIt w = B.concat ["{", w , "}"]

brackVar x = parseChar '{' x >> nested x

parseStr = parseStrRaw `wrapWith` Word

parseStrRaw s = do 
  str <- eatChar '"' s
  loc <- B.elemIndex '"' str
  let (w,r) = B.splitAt loc str
  if escaped loc str then do (w1, v) <- parseStrRaw r
                             let nw = B.snoc (B.take (B.length w - 1) w) '"'
                             return (B.append nw w1, v)
                     else return (w, B.tail r)

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

escaped v s = escaped' v
 where escaped' !i = if i <= 0 then False 
                               else (B.index s (i-1) == '\\') && not (escaped' (i-1))

mkNoSub s = NoSub s (runParse s)

parseExpand = parseLit "{*}" .>> (parseToken `wrapWith` Expand)

parseNoSub = nested `wrapWith` mkNoSub

nested s = do ind <- match 0 0 False
              let (w,r) = B.splitAt ind s
              return (B.tail w, B.tail r)
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
        (escaped 1 "\\\"") ~? "pre-slashed quote should be escaped",
        checkFalse "non-slashed quote not escaped"  (escaped 1 " \""),
        checkFalse "non-slashed quote not escaped"  (escaped 1 " \""),
        (escaped 2 " \\\"") ~? "pre-slashed quote should be escaped",
        checkFalse "non-slashed quote not escaped"  (escaped 2 "  \"")
  ]
 where checkFalse str val = TestCase $ assertBool str (not val)

bp = id 
mklit = Word . bp
mkwd = Word . bp

parseStrTests = TestList [
      "Escaped works" ~: (mkwd "Oh \"yeah\" baby.", "") ?=? "\"Oh \\\"yeah\\\" baby.\"",
      "Parse Str with leftover" ~: (mkwd "Hey there.", " 44") ?=? "\"Hey there.\" 44",
      "Parse Str with dolla" ~: (mklit "How about \\$44?", "") ?=? "\"How about \\$44?\"",
      "bad parse1" ~: badParse "What's new?"
   ]
 where (?=?) res str = Just res ~=? parseStr (bp str)
       badParse str = Nothing ~=? parseStr (bp str)

brackVarTests = TestList [
      "Simple" ~: ("data", "") ?=? "{data}",
      "With spaces" ~: (" a b c d ",  " ") ?=? "{ a b c d } ",
      "With esc" ~: (" \\} yeah! ", " ") ?=? "{ \\} yeah! } ",
      "bad parse" ~: badParse "{ oh no",
      "bad parse" ~: badParse "pancake"
   ]
 where (?=?) res str = Just res ~=? brackVar (bp str)
       badParse str = Nothing ~=? brackVar (bp str)

getInterpTests = TestList [
    "Escaped $ works" ~: noInterp "a \\$variable",
    "Bracket interp 1" ~: ("", mkvar "booga", "") ?=? "${booga}",
    "Bracket interp 2" ~: ("", mkvar "oh yeah!", "") ?=? "${oh yeah!}",
    "Bracket interp 3" ~: (" ", mkvar " !?! ", " ") ?=? " ${ !?! } ",
    "global namespace" ~: ("", mkvar "::booga", "") ?=? "$::booga",
    ":::"              ~: noInterp "$:::booga",
    "some namespace" ~: ("", mkvar "log::booga", "") ?=? "$log::booga", -- TODO
    "unescaped $ works" ~:
          ("a ", mkvar "variable", "")  ?=? "a $variable",
    "escaped $ works" ~:
          ("a \\$ ", mkvar "variable", "")  ?=? "a \\$ $variable",
    "escaped $ works 2" ~:
          noInterp  "you deserve \\$44.",
    "adjacent interp works" ~:
          ("", mkvar "var", "$bar$car")  ?=? "$var$bar$car",
    "interp after escaped dolla" ~:
          ("a \\$", mkvar "name", " guy")  ?=? "a \\$$name guy",
    "interp after dolla" ~:
          ("you have $", mkvar "dollars", "")  ?=? "you have $$dollars",
    "Escaped ["   ~: noInterp "a \\[sub] thing.",
    "Trailing bang" ~: ("", mkvar "var",  "!" ) ?=? "$var!",
    "basic arr" ~: ("", mkvar "boo(4)", " " ) ?=? "$boo(4) ",
    "basic arr2" ~: (" ", mkvar "boo(4)", " " ) ?=? " $boo(4) ",
    "basic arr3" ~: ("", mkvar "boo( 4,5 )", " " ) ?=? "$boo( 4,5 ) ",
    "Escaped []"   ~: noInterp "a \\[sub\\] thing.",
    "Lone $ works" ~: noInterp "a $ for the head of each rebel!",
    "Escaped lone $ works" ~: noInterp "a \\$ for the head of each rebel!",
    "unescaped $ after esc works" ~:
          ("a \\$", mkvar "variable", "") ?=? "a \\$$variable",
    "Escaped [] crazy" ~:
       ("a ",Right (mkwd "sub",[mklit "quail [puts 1]"]), " thing.") ?=? "a [sub \"quail [puts 1]\" ] thing."
  ]
 where noInterp str = Nothing ~=? getInterp (bp str)
       (?=?) res str = Just res ~=? getInterp (bp str)
       mkvar w = Left w

doInterpTests = TestList [
    "dollar escape"  ~: "oh $ yeah" ?!= "oh \\$ yeah",
    "brace escape"  ~: "oh [ yeah" ?!= "oh \\[ yeah",
    "tab escape"     ~: "a \t tab"  ?!= "a \\t tab",
    "slash escape"     ~: "slash \\\\ party"  ?!= "slash \\\\\\\\ party",
    "subcom"   ~: ("some ", Right (mkwd "cmd", []), "") ?=? "some [cmd]",
    "newline escape" ~: "\nline\n"  ?!= "\\nline\\n"
  ]
 where (?=?) res str = Right res ~=? doInterp (bp str)
       (?!=) res str = Left (bp res) ~=? doInterp (bp str)

getWordTests = TestList [
     "Simple" ~: badword "",
     "Simple2" ~: (mkwd "$whoa", "") ?=? "$whoa",
     "Simple with bang" ~: (mkwd "whoa!", " ") ?=? "whoa! "
  ]
 where badword str = Nothing ~=? wordToken (bp str)
       (?=?) res str = Just res ~=? wordToken (bp str)

nestedTests = TestList [
  "Fail nested" ~: "  {       the end" `should_fail` (),
  "Pass nested" ~: "{  { }}" `should_be` "  { }",
  "Pass empty nested" ~: "{ }" `should_be` " ",
  "Fail nested" ~: "  { {  }" `should_fail` (),
  "Pass escape" ~: "{ \\{ }" `should_be` " \\{ ",
  "Pass escape 2" ~: "{ \\{ \\{ }" `should_be` " \\{ \\{ ",
  "Pass escape 3" ~: "{ \\\\}" `should_be` " \\\\",
  "Pass escape 4" ~: "{ \\} }" `should_be` " \\} "
  ,"no bracks" ~: "happy" `should_fail` ()
 ]
 where should_be act exp = Just (bp exp, B.empty) ~=? nested (bp act)
       should_fail act () = Nothing ~=? nested (bp act)

parseTokensTests = TestList [
     " x " ~: "x" ?=> ([mkwd "x"], "")
     ," x y " ~: " x y " ?=> ([mkwd "x", mkwd "y"], " ")
     ,"x y" ~: "x y" ?=> ([mkwd "x", mkwd "y"], "")
     ,"x { y 0 }" ~: "x { y 0 }" ?=> ([mkwd "x", nosub " y 0 "], "")
     ,"x {y 0}" ~: "x {y 0}" ?=> ([mkwd "x", nosub "y 0"], "")
   ]
 where (?=>) str (res,r) = Just (res, bp r) ~=? parseTokens (bp str)
       nosub s = mkNoSub (bp s)

parseListTests = TestList [
     " x "     ~: " x "   ?=> ["x"]
     ,""       ~: ""      ?=> []
     ,"\t \t " ~: "\t \t" ?=> []
     ," x y "  ~: " x y " ?=> ["x", "y"]
     ,"x y" ~: "x y" ?=> ["x", "y"]
     ,"x { y 0 }" ~: "x { y 0 }" ?=> ["x", " y 0 "]
     ,"x [puts yay]" ~: "x [puts yay]" ?=> ["x", "[puts", "yay]"]
     ," y { \\{ \\{ \\{ } { x }" ~: " y { \\{ \\{ \\{ } { x }" ?=> ["y", " \\{ \\{ \\{ ", " x "]
     , "unmatched fail" ~: fails " { { "
     ,"x {y 0}" ~: "x {y 0}" ?=> ["x", "y 0"]
     ,"with nl" ~: "x  1 \n y 2 \n z 3" ?=> ["x", "1", "y", "2", "z", "3"]
     ,"escaped1" ~: "x \\{ z" ?=> ["x", "\\{", "z"]
   ]
 where (?=>) str res = Just res ~=? parseList (bp str)
       fails str = Nothing ~=? parseList (bp str)

parseVarRefTests = TestList [
     no_parse ""
    ,"standard" ~: "boo" ?=> ("boo", "")
    ,"global" ~: "::boo" ?=> ("::boo", "")
    ,"arr1" ~: "boo(one) " ?=> ("boo(one)", " ")
    ,"ns arr1" ~: "::big::boo(one) " ?=> ("::big::boo(one)", " ")
    ,"::big(3)$::boo(one)" ?=> ("::big(3)", "$::boo(one)")
    , "triple" ~: "::one::two::three" ?=> ("::one::two::three","")
    , "brack" ~: "::one::{t o}::three" ?=> ("::one::t o::three","")
    , "mid paren" ~: "::one::two(1)::three" ?=> ("::one::two(1)", "::three")
   ]
 where (?=>) str (p,r) = Just (bp p, bp r) ~=? parseVarRef (bp str) 
       no_parse str = Nothing ~=? parseVarRef (bp str)

runParseTests = TestList [
     "one token" ~: (pr ["exit"]) ?=? "exit",
     "multi-line" ~: (pr ["puts", "44"]) ?=? " puts \\\n   44",
     "escaped space" ~: (pr ["puts", "\\ "]) ?=? " puts \\ ",
     "empty" ~: ([],"") ?=? " ",
     "empty2" ~: ([],"") ?=? "",
     "unmatched" ~: badword "{ { }",
     "a b " ~: (pr ["a", "b"]) ?=? "a b ",
     "two vars" ~: (pr ["puts", "$one$two"]) ?=? "puts $one$two",
     "brack" ~: (pr ["puts", "${oh no}"]) ?=? "puts ${oh no}",
     "arr 1" ~: (pr ["set","buggy(4)", "11"]) ?=? "set buggy(4) 11",
     "arr 2" ~: (pr ["set","buggy($bean)", "11"]) ?=? "set buggy($bean) 11",
     "arr 3" ~: (pr ["set","buggy($bean)", "$koo"]) ?=? "set buggy($bean) $koo"
     ,"arr 4" ~: (pr ["set","buggy($bean)", "${wow}"]) ?=? "set buggy($bean) ${wow}"
     ,"quoted ws arr" ~: (pr ["set","arr(1 2)", "4"]) ?=? "set \"arr(1 2)\" 4"
     ,"hashed num" ~: (pr ["uplevel", "#1", "exit"]) ?=? "uplevel #1 exit"
     -- not yet TODO
     -- ,"unquoted ws arr" ~: (pr ["puts","$arr(1 2)"]) ?=? "puts $arr(1 2)"
    ,"expand" ~: ([(mkwd "incr", [Expand (mkwd "$boo")])], "") ?=? "incr {*}$boo"
    ,"no expand" ~: ([(mkwd "incr", [mkNoSub "*", mkwd "$boo"])], "") ?=? "incr {*} $boo"
  ]
 where badword str = Nothing ~=? runParse (bp str)
       (?=?) (res,r) str = Just (res, bp r) ~=? runParse (bp str)
       pr (x:xs) = ([(mkwd x, map mkwd xs)], "")
       pr []     = error "bad test!"

bsParseTests = TestList [ nestedTests, testEscaped, brackVarTests,
                   parseStrTests, getInterpTests, getWordTests, doInterpTests,
                   parseTokensTests, parseListTests, runParseTests, parseVarRefTests]

-- # ENDTESTS # --
