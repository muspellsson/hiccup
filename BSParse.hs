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
type ParseMonad = Either String
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
parseStatement = eatAndNext `orElse` parseEof `orElse` parseTokens
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
   h <- safeHead "token" str
   case h of
     '{'  -> (parseExpand `orElse` parseNoSub) str
     '['  -> (parseSub `wrapWith` Subcommand) str
     '"'  -> parseStr str
     '\\' -> handleEsc str
     _    -> wordToken str

handleEsc :: Parser TclWord
handleEsc = line_continue `orElse` esc_word
 where line_continue = parseLit "\\\n" .>> eatSpaces .>> parseToken
       esc_word = (chain [escaped_char, tryGet wordTokenRaw]) `wrapWith` Word

parseList s = parseList_ s >>= return . fst
parseList_ :: Parser [BString]
parseList_ = eatWhite .>> (parseEof `orElse` multi plistItem)
 where isWhite = (`elem` " \t\n")
       eatWhite st = return ((), B.dropWhile isWhite st)
       plistItem = listDisp `pass` eatWhite

listDisp str = do h <- safeHead "list item" str
                  case h of
                   '{' -> pnested str
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


safeHead r s = if B.null s then fail ("expected " ++ r ++ ", got eof") else return (B.head s)
{-# INLINE safeHead #-}

doInterp str = case getInterp str of
                   Left _ -> Left (escapeStr str)
                   Right (pr,s,r) -> Right (escapeStr pr, s, r)

(.>-) f w s = wrapWith f w s

-- TODO: UGLY
getInterp str = do
   loc <- case B.findIndex (\x -> x == '$' || x == '[') str of
            Just v -> return v
            Nothing -> fail "no matching $ or ["
   let locval = B.index str loc
   if escaped loc str
     then dorestfrom loc locval
     else let (pre,aft) = B.splitAt loc str 
              pfun = (doVarParse .>-  Left) `orElse` (parseSub .>- Right) 
              res = pfun aft >>= \(v,rest) -> return (pre, v, rest) 
          in res `mplus` dorestfrom loc locval
 where dorestfrom loc lval = do (p,v,r) <- getInterp (B.drop (loc+1) str)
                                return (B.append (B.take loc str) (B.cons lval p), v, r)

doVarParse :: Parser BString
doVarParse = pchar '$' .>> parseVarRef

parseVarRef :: Parser BString
parseVarRef = chain [ parseVarTerm `orElse` getNS
                      ,tryGet parseVarRef
                      ,tryGet parseInd ]

getNS = chain [parseLit "::", parseVarTerm, tryGet getNS]

parseVarTerm :: Parser BString
parseVarTerm = getVar `orElse` braceVar
 where getVar = getPred1 wordChar "word"

parseInd :: Parser BString
parseInd str = do 
    pchar '(' str
    ind <- case B.elemIndex ')' str of
             Just v -> return v
             Nothing -> fail "Couldn't find matching \")\""
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


 -- Toys for later
parseInt :: Parser Int 
parseInt s = case B.readInt s of
                Just x -> return x
                Nothing -> fail "expected int"

data OTok = TokNum Int | TokOp Oper | TokPar [OTok] deriving (Eq,Show)
data Oper = OpPlus | OpMinus | OpTimes deriving (Eq,Show)
parseOp = ((op '*' OpTimes) `orElse` parseOpPlus `orElse` parseOpMinus) `wrapWith` TokOp
 where op c v = eatSpaces .>> parseChar c .>> \s -> return (v,s) 
       parseOpPlus = op '+' OpPlus
       parseOpMinus = op '-' OpMinus

tokExpr :: Parser [OTok]
tokExpr = parseMany1 toks
 where toks = parseOp `orElse` intTok `orElse` parenTok 
       intTok = (eatSpaces .>> parseInt) `wrapWith` TokNum
       parenTok = (parseParen tokExpr) `wrapWith` TokPar
       parseParen :: Parser t -> Parser t
       parseParen p = (eatSpaces .>> pchar '(' .>> p) `pass` (eatSpaces .>> pchar ')')
   
{-

parseDouble :: Parser Double
parseDouble s = do
    (ds, r) <- chain [parseNums, parseChar '.', parseNums] s
    return (read (B.unpack ds), r)
 where parseNums = getPred1 (inRange ('0','9'))

-}
         
parseOneOf !cl = parseCharPred (`elem` cl) ("one of " ++ show cl)
parseChar :: Char -> Parser BString
parseChar !c = parseCharPred (== c) (show c)

parseAny = parseCharPred (const True) "any char"
parseCharPred pred exp s = case B.uncons s of
                            Nothing    -> failStr "eof"
                            Just (h,t) -> if pred h then return (B.singleton h,t)
                                                    else failStr (show h)
 where failStr what = fail $ "expected " ++ exp ++ ", got " ++ what
{-# INLINE parseCharPred #-}

parseEof s = if B.null s 
               then return ([], s) 
               else fail $ "expected eof, got " ++ show (B.head s)

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

-- TODO: Document and possibly rename 'multi'
multi :: Parser t -> Parser [t]
multi p = pjoin (:) p (parseEof `orElse` (multi p))
{-# INLINE multi #-}

brackets p = (pchar '[' .>> p) `pass` (eatSpaces .>> pchar ']')

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

getPred1 p desc s = if B.null w then fail ("wanted " ++ desc ++ ", got eof") else return $! (w,n)
 where (w,n) = B.span p s


tryGet fn s = (fn `orElse` (\_ -> return (B.empty, s))) s

wrapWith fn wr s = fn s >>= \(!w,r) -> return (wr w, r) 
{-# INLINE wrapWith #-}

wordToken = wordTokenRaw `wrapWith` Word
wordTokenRaw = parseMany1 ((chain [pchar '$', parseVarBody, tryGet parseInd]) `orElse` pthing) `wrapWith` B.concat
 where pthing = getPred1 (`notElem` " ${}[]\n\t\\;") "inner word"
 -- TODO: Rename pthing

parseVarBody = (braceVar `wrapWith` braceIt) `orElse` pthing
 where braceIt w = B.concat ["{", w , "}"]
       pthing = getPred1 (`notElem` "( ${}[]\n\t\\;") "inner word"

braceVar = pnested

parseStr = parseStrRaw `wrapWith` Word

pchar = parseChar

parseStrRaw = pchar '"' .>> (inside `wrapWith` B.concat) `pass` (pchar '"')
 where noquotes = getPred1 (`notElem` "\"\\") "non-quote chars"
       inside = parseMany (noquotes `orElse` escaped_char)

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

parseNoSub = pnested `wrapWith` mkNoSub

escaped_char = chain [pchar '\\', parseAny]

pnested = pchar '{' .>> nest_filling `pass` pchar '}'
 where inner = escaped_char `orElse` braces `orElse` nobraces
       nest_filling = tryGet ((parseMany inner) `wrapWith` B.concat)
       braces = chain [pchar '{', nest_filling, pchar '}']
       nobraces = getPred1 (`notElem` "{}\\") "non-brace chars"

-- # TESTS # --

testEscaped = TestList [
        (escaped 1 "\\\"") ~? "pre-slashed quote should be escaped",
        checkFalse "non-slashed quote not escaped"  (escaped 1 " \""),
        checkFalse "non-slashed quote not escaped"  (escaped 1 " \""),
        (escaped 2 " \\\"") ~? "pre-slashed quote should be escaped",
        checkFalse "non-slashed quote not escaped"  (escaped 2 "  \"")
  ]
 where checkFalse str val = TestCase $ assertBool str (not val)

should_fail_ act _ = let res = case act of 
                                 Left _ -> True
                                 _      -> False
                     in TestCase $ assertBool "should fail" res


mklit = Word
mkwd = Word

parseStrTests = "parseStr" ~: TestList [
      "Escaped works" ~: (mkwd "Oh \\\"yeah\\\" baby.", "") ?=? "\"Oh \\\"yeah\\\" baby.\"",
      "Parse Str with leftover" ~: (mkwd "Hey there.", " 44") ?=? "\"Hey there.\" 44",
      "Parse Str with dolla" ~: (mklit "How about \\$44?", "") ?=? "\"How about \\$44?\"",
      "bad parse1" ~: "What's new?" `should_fail` ()
   ]
 where (?=?) res str = Right res ~=? parseStr str
       should_fail str _  = (parseStr str) `should_fail_` ()

braceVarTests = TestList [
      "Simple" ~: ("data", "") ?=? "{data}",
      "With spaces" ~: (" a b c d ",  " ") ?=? "{ a b c d } ",
      "With esc" ~: (" \\} yeah! ", " ") ?=? "{ \\} yeah! } ",
      "bad parse" ~: "{ oh no" `should_fail` (),
      "bad parse" ~: "pancake" `should_fail` ()
   ]
 where (?=?) res str = Right res ~=? braceVar str
       should_fail str _  = (braceVar str) `should_fail_` ()

getInterpTests = "getInterp" ~: TestList [
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
 where noInterp str = (getInterp str) `should_fail_` ()
       (?=?) res str = Right res ~=? getInterp str
       mkvar w = Left w

doInterpTests = TestList [
    "dollar escape"  ~: "oh $ yeah" ?!= "oh \\$ yeah",
    "brace escape"  ~: "oh [ yeah" ?!= "oh \\[ yeah",
    "tab escape"     ~: "a \t tab"  ?!= "a \\t tab",
    "slash escape"     ~: "slash \\\\ party"  ?!= "slash \\\\\\\\ party",
    "subcom"   ~: ("some ", Right (mkwd "cmd", []), "") ?=? "some [cmd]",
    "newline escape" ~: "\nline\n"  ?!= "\\nline\\n"
  ]
 where (?=?) res str = Right res ~=? doInterp str
       (?!=) res str = Left res ~=? doInterp str

getWordTests = "wordToken" ~: TestList [
     "empty" ~: "" `should_fail` ()
     ,"Simple2" ~: (mkwd "$whoa", "") ?=? "$whoa"
     ,"Simple with bang" ~: (mkwd "whoa!", " ") ?=? "whoa! "
     ,"braced, then normal" ~: (mkwd "${x}$x", "") ?=? "${x}$x"
     ,"non-var, then var" ~: (mkwd "**$x", "") ?=? "**$x"
     ,"non-var, then var w/ space" ~: (mkwd "**${a b}", "") ?=? "**${a b}"
  ]
 where should_fail str _ = (wordToken str) `should_fail_` ()
       (?=?) res str = Right res ~=? wordToken str

nestedTests = "nested" ~: TestList [
  "Fail nested" ~: "  {       the end" `should_fail` (),
  "Pass nested" ~: "{  { }}" `should_be` "  { }",
  "Pass empty nested" ~: "{ }" `should_be` " ",
  "Fail nested" ~: "  { {  }" `should_fail` (),
  "Pass escape" ~: "{ \\{ }" `should_be` " \\{ ",
  "Pass escape 2" ~: "{ \\{ \\{ }" `should_be` " \\{ \\{ ",
  "Pass escape 3" ~: "{ \\\\}" `should_be` " \\\\",
  "Pass escape 4" ~: "{ \\} }" `should_be` " \\} "
  ,"no braces" ~: "happy" `should_fail` ()
 ]
 where should_be act exp = Right (exp, B.empty) ~=? pnested act
       should_fail act _ = (pnested act) `should_fail_` ()

parseTokensTests = "parseTokens" ~: TestList [
     " x " ~: "x" ?=> ([mkwd "x"], "")
     ," x y " ~: " x y " ?=> ([mkwd "x", mkwd "y"], " ")
     ,"x y" ~: "x y" ?=> ([mkwd "x", mkwd "y"], "")
     ,"x { y 0 }" ~: "x { y 0 }" ?=> ([mkwd "x", nosub " y 0 "], "")
     ,"x {y 0}" ~: "x {y 0}" ?=> ([mkwd "x", nosub "y 0"], "")
   ]
 where (?=>) str (res,r) = Right (res, r) ~=? parseTokens str
       nosub = mkNoSub

parseListTests = "parseList" ~: TestList [
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
 where (?=>) str res = Right res ~=? parseList str
       fails str = (parseList str) `should_fail_` ()

parseVarRefTests = "parseVarRef" ~: TestList [
     "empty string" ~: "" `should_fail` ()
    ,"standard" ~: "boo" ?=> ("boo", "")
    ,"global" ~: "::boo" ?=> ("::boo", "")
    ,"arr1" ~: "boo(one) " ?=> ("boo(one)", " ")
    ,"ns arr1" ~: "::big::boo(one) " ?=> ("::big::boo(one)", " ")
    ,"::big(3)$::boo(one)" ?=> ("::big(3)", "$::boo(one)")
    , "triple" ~: "::one::two::three" ?=> ("::one::two::three","")
    , "brace" ~: "::one::{t o}::three" ?=> ("::one::t o::three","")
    , "mid paren" ~: "::one::two(1)::three" ?=> ("::one::two(1)", "::three")
   ]
 where (?=>) str (p,r) = Right (p, r) ~=? parseVarRef str
       should_fail a () = (parseVarRef a) `should_fail_` ()


runParseTests = "runParse" ~: TestList [
     "one token" ~: (pr ["exit"]) ?=? "exit",
     "multi-line" ~: (pr ["puts", "44"]) ?=? " puts \\\n   44",
     "escaped space" ~: (pr ["puts", "\\ "]) ?=? " puts \\ ",
     "empty" ~: ([],"") ?=? " ",
     "empty2" ~: ([],"") ?=? "",
     "unmatched" ~: "{ { }" `should_fail` (),
     "a b " ~: "a b " `should_be` ["a", "b"],
     "two vars" ~: (pr ["puts", "$one$two"]) ?=? "puts $one$two",
     "brace" ~: (pr ["puts", "${oh no}"]) ?=? "puts ${oh no}",
     "arr 1" ~: (pr ["set","buggy(4)", "11"]) ?=? "set buggy(4) 11",
     "arr 2" ~: (pr ["set","buggy($bean)", "11"]) ?=? "set buggy($bean) 11",
     "arr 3" ~: (pr ["set","buggy($bean)", "$koo"]) ?=? "set buggy($bean) $koo"
     ,"arr 4" ~: (pr ["set","buggy($bean)", "${wow}"]) ?=? "set buggy($bean) ${wow}"
     ,"quoted ws arr" ~: (pr ["set","arr(1 2)", "4"]) ?=? "set \"arr(1 2)\" 4"
     ,"hashed num" ~: (pr ["uplevel", "#1", "exit"]) ?=? "uplevel #1 exit"
     ,"unquoted ws arr" ~: (pr ["puts","$arr(1 2)"]) ?=? "puts $arr(1 2)"
    ,"expand" ~: ([(mkwd "incr", [Expand (mkwd "$boo")])], "") ?=? "incr {*}$boo"
    ,"no expand" ~: ([(mkwd "incr", [mkNoSub "*", mkwd "$boo"])], "") ?=? "incr {*} $boo"
  ]
 where should_fail str () = (runParse str) `should_fail_` ()
       should_be str res = Right (pr res) ~=? (runParse str)
       (?=?) (res,r) str = Right (res, r) ~=? runParse str
       pr (x:xs) = ([(mkwd x, map mkwd xs)], "")
       pr []     = error "bad test!"

futureTests = "future" ~: TestList [intTests, tokTests] where
  tokTests = TestList [
      "3 + 4" `should_be` [TokNum 3, TokOp OpPlus, TokNum 4]
      ,"3+4" `should_be` [TokNum 3, TokOp OpPlus, TokNum 4]
      ,"3-4" `should_be` [TokNum 3, TokOp OpMinus, TokNum 4]
      ,"(3+4)" `should_be` [TokPar [TokNum 3, TokOp OpPlus, TokNum 4]]
    ] where should_be dat res = (B.unpack dat) ~: Right (res,"") ~=? tokExpr dat

  intTests = TestList [
       "44" `should_be` 44
       ,"9" `should_be` 9
       ,"catfish" `should_fail` ()
    ] where should_be dat res = Right (res,"") ~=? parseInt dat
            should_fail dat () = (parseInt dat) `should_fail_` ()
      

bsParseTests = TestList [ nestedTests, testEscaped, braceVarTests,
                   parseStrTests, getInterpTests, getWordTests, doInterpTests,
                   parseTokensTests, parseListTests, runParseTests, 
                   parseVarRefTests, futureTests]

-- # ENDTESTS # --
