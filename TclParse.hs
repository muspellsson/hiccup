{-# LANGUAGE BangPatterns,OverloadedStrings #-}

module TclParse ( TclWord(..)
                 ,doInterp 
                 ,runParse 
                 ,parseList 
                 ,doVarParse
                 ,parseSub
                 ,TokCmd
                 ,tclParseTests
                )  where
 

import BSParse
import Util 
import qualified Data.ByteString.Char8 as B
import Test.HUnit

data TclWord = Word !B.ByteString 
             | Subcommand TokCmd 
             | NoSub !B.ByteString (Result [TokCmd])
             | Expand TclWord deriving (Show,Eq)

type TokCmd = (TclWord, [TclWord])


runParse :: Parser [TokCmd]
runParse = parseStatements `wrapWith` asCmds
 where asCmds lst = [ let (c:a) = x in (c,a) | x <- lst, not (null x)]


parseStatements :: Parser [[TclWord]]
parseStatements = multi1 (eatSpaces .>> parseStatement) `pass` parseEof
  

parseStatement :: Parser [TclWord]
parseStatement = choose [eatAndNext, parseEof, parseTokens]
 where stmtSep = parseOneOf "\n;" .>> eatSpaces 
       eatAndNext = (stmtSep `orElse` parseComment) .>> parseStatement

parseComment = pchar '#' .>> parseMany (ignored `orElse` (eat escapedChar)) .>> eatSpaces
 where ignored = eat (getPred1 (`notElem` "\n\\") "not newline or slash")
       eat p = p .>> emit ()

parseTokens :: Parser [TclWord]
parseTokens = parseMany1 (eatSpaces .>> parseToken)

parseToken :: Parser TclWord
parseToken str = do 
   h <- safeHead "token" str
   case h of
     '{'  -> (parseExpand `orElse` parseNoSub) str
     '['  -> (parseSub `wrapWith` Subcommand) str
     '"'  -> (parseStr `wrapWith` Word) str
     '\\' -> handleEsc str
     _    -> (wordToken `wrapWith` Word) str
 where parseNoSub = parseBlock `wrapWith` mkNoSub
       parseExpand = parseLit "{*}" .>> (parseToken `wrapWith` Expand)

mkNoSub s = NoSub s (runParse s)

parseSub :: Parser TokCmd
parseSub s = do 
  (p,aft) <- brackets parseTokens $ s
  case p of
    [] -> fail "empty subcommand"
    (ph:pt) -> return ((ph,pt), aft)

handleEsc :: Parser TclWord
handleEsc = line_continue `orElse` esc_word
 where line_continue = parseLit "\\\n" .>> eatSpaces .>> parseToken
       esc_word = (chain [escapedChar, tryGet wordToken]) `wrapWith` Word

-- List parsing
parseList s = parseList_ s >>= return . fst
parseList_ :: Parser [BString]
parseList_ = eatWhite .>> (parseEof `orElse` multi1 plistItem)
 where isWhite = (`elem` " \t\n")
       eatWhite st = return ((), B.dropWhile isWhite st)
       plistItem = listDisp `pass` eatWhite

listDisp str = do h <- safeHead "list item" str
                  case h of
                   '{' -> parseBlock str
                   '"' -> parseStr str 
                   _   -> getListItem str

getListItem s = if B.null w then fail "can't parse list item" else return (w,n)
 where (w,n) = B.splitAt (listItemEnd s) s

listItemEnd s = inner 0 False where 
   inner i esc = if i == B.length s then i
                     else if esc then inner (i+1) False
                           else case B.index s i of
                                  '\\' -> inner (i+1) True
                                  v  -> if v `B.elem` "{}\" \t\n" then i else inner (i+1) False


-- TODO: UGLY
getInterp :: Parser (BString, Either BString TokCmd)
getInterp str = do
   loc <- case B.findIndex (\x -> x == '$' || x == '[') str of
            Just v -> return v
            Nothing -> fail "no matching $ or ["
   if escaped loc str
     then dorestfrom loc str
     else let res = pjoin (,) (parseLen loc) interpItem
          in (res `orElse` dorestfrom loc) str
 where dorestfrom loc = (pjoin (\s (p,v) -> (B.append s p, v)) (parseLen (loc+1)) getInterp) 
       interpItem = (doVarParse `wrapWith` Left) `orElse` (parseSub `wrapWith` Right) 

doVarParse :: Parser BString
doVarParse = pchar '$' .>> parseVarRef

parseVarRef :: Parser BString
parseVarRef = chain [ parseVarTerm `orElse` getNS
                      ,tryGet parseVarRef
                      ,tryGet parseInd ]
 where getNS = chain [parseLit "::", parseVarTerm, tryGet getNS]

parseVarTerm :: Parser BString
parseVarTerm = getVar `orElse` braceVar
 where getVar = getPred1 wordChar "word"

parseInd :: Parser BString
parseInd = chain [pchar '(', getPred (/= ')'), pchar ')']

wordToken = consumed (parseMany1 (someVar `orElse` inner `orElse` someCmd))
 where simple = getPred1 (`notElem` " $[]\n\t;\\") "inner word"
       inner = consumed (parseMany1 (simple `orElse` escapedChar)) 
       someVar = chain_ [pchar '$', parseVarBody, tryGet parseInd]
       someCmd = consumed (brackets parseTokens)

parseVarBody = (braceVar `wrapWith` braceIt) `orElse` pthing
 where braceIt w = B.concat ["{", w , "}"]
       pthing = getPred1 (`notElem` "( ${}[]\n\t;") "inner word"

escaped v s = escaped' v
 where escaped' !i = if i <= 0 
                         then False 
                         else (B.index s (i-1) == '\\') && not (escaped' (i-1))

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


tclParseTests = TestList [ runParseTests, 
                           parseVarRefTests,
                           parseListTests,
                           parseTokensTests,
                           wordTokenTests, 
                           getInterpTests,
                           doInterpTests,
                           commentTests,
                           testEscaped ]

commentTests = "parseComment" ~: TestList [
   "# hey there" `should_be` ""
   ,"# hey there \n FISH" `should_be` "\n FISH"
   ,"# hey there \\\n FISH" `should_be` ""
 ]
  where should_be str res = (B.unpack str) ~: Right ((),res) ~=? parseComment str

runParseTests = "runParse" ~: TestList [
     "one token" ~: (pr ["exit"]) ?=? "exit",
     "multi-line" ~: (pr ["puts", "44"]) ?=? " puts \\\n   44",
     "escaped space" ~: (pr ["puts", "\\ "]) ?=? " puts \\ ",
     "empty" ~: ([],"") ?=? " ",
     "empty2" ~: ([],"") ?=? "",
     "unmatched" ~: "{ { }" `should_fail` (),
     "a b " ~: "a b " `should_be` ["a", "b"],
     "two vars" ~: (pr ["puts", "$one$two"]) ?=? "puts $one$two",
     "brace inside" ~: (pr ["puts", "x{$a}x"]) ?=? "puts x{$a}x",
     "brace" ~: (pr ["puts", "${oh no}"]) ?=? "puts ${oh no}",
     "arr 1" ~: (pr ["set","buggy(4)", "11"]) ?=? "set buggy(4) 11",
     "arr 2" ~: (pr ["set","buggy($bean)", "11"]) ?=? "set buggy($bean) 11",
     "arr 3" ~: (pr ["set","buggy($bean)", "$koo"]) ?=? "set buggy($bean) $koo"
     ,"arr 4" ~: (pr ["set","buggy($bean)", "${wow}"]) ?=? "set buggy($bean) ${wow}"
     ,"arr w/ esc index" ~: (pr ["set","x(\\))", "4"]) ?=? "set x(\\)) 4"
     ,"quoted ws arr" ~: (pr ["set","arr(1 2)", "4"]) ?=? "set \"arr(1 2)\" 4"
     ,"hashed num" ~: (pr ["uplevel", "#1", "exit"]) ?=? "uplevel #1 exit"
     ,"command appended" ~: (pr ["set", "val", "x[nop 4]"]) ?=? "set val x[nop 4]"
     ,"unquoted ws arr" ~: (pr ["puts","$arr(1 2)"]) ?=? "puts $arr(1 2)"
     ,"expand" ~: ([(Word "incr", [Expand (Word "$boo")])], "") ?=? "incr {*}$boo"
     ,"no expand" ~: ([(Word "incr", [mkNoSub "*", Word "$boo"])], "") ?=? "incr {*} $boo"
  ]
 where should_fail str () = (runParse str) `should_fail_` ()
       should_be str res = Right (pr res) ~=? (runParse str)
       (?=?) (res,r) str = Right (res, r) ~=? runParse str
       pr (x:xs) = ([(Word x, map Word xs)], "")
       pr []     = error "bad test!"

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
       ("a ",Right (Word "sub",[Word "quail [puts 1]"]), " thing.") ?=? "a [sub \"quail [puts 1]\" ] thing."
  ]
 where noInterp str = (getInterp str) `should_fail_` ()
       (?=?) (a,b,c) str = Right ((a,b),c) ~=? getInterp str
       mkvar w = Left w

wordTokenTests = "wordToken" ~: TestList [
     "empty" ~: "" `should_fail` ()
     ,"Simple2" ~: ("$whoa", "") ?=? "$whoa"
     ,"Simple with bang" ~: ("whoa!", " ") ?=? "whoa! "
     ,"braced, then normal" ~: valid_word "${x}$x"
     ,"normal, then cmd" ~: ("fish[nop 5]", "") ?=? "fish[nop 5]"
     ,"non-var, then var" ~: ("**$x", "") ?=? "**$x"
     ,"non-var, then var w/ space" ~: valid_word "**${a b}"
     ,"escaped" ~: valid_word "x\\ y"
     ,"inner bracket" ~: valid_word "a{{" 
     ,"inner bracket 2" ~: valid_word "a}|" 
  ]
 where should_fail str _ = (wordToken str) `should_fail_` ()
       (?=?) res str = Right res ~=? wordToken str
       should_be str res = Right res ~=? wordToken str
       valid_word x = x `should_be` (x,"")

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
    , "\\( in index" ~: "x(\\()" ?=> ("x(\\()", "")
   ]
 where (?=>) str (p,r) = Right (p, r) ~=? parseVarRef str
       should_fail a () = (parseVarRef a) `should_fail_` ()

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

testEscaped = TestList [
        (escaped 1 "\\\"") ~? "pre-slashed quote should be escaped",
        checkFalse "non-slashed quote not escaped"  (escaped 1 " \""),
        checkFalse "non-slashed quote not escaped"  (escaped 1 " \""),
        (escaped 2 " \\\"") ~? "pre-slashed quote should be escaped",
        checkFalse "non-slashed quote not escaped"  (escaped 2 "  \"")
  ]
 where checkFalse str val = TestCase $ assertBool str (not val)

parseListTests = "parseList" ~: TestList [
     " x " `should_be` ["x"]
     ,""   `should_be` []
     ,"\t \t" `should_be` []
     ," x y " `should_be` ["x", "y"]
     ,"x y" ~: "x y" ?=> ["x", "y"]
     ,"x { y {z}}" `should_be` ["x", " y {z}"]
     ,"x { y 0 }" ~: "x { y 0 }" ?=> ["x", " y 0 "]
     ,"x [puts yay]" ~: "x [puts yay]" ?=> ["x", "[puts", "yay]"]
     ," y { \\{ \\{ \\{ } { x }" `should_be` ["y", " \\{ \\{ \\{ ", " x "]
     , "unmatched fail" ~: fails " { { "
     ,"x {y 0}" ~: "x {y 0}" ?=> ["x", "y 0"]
     ,"with nl" ~: "x  1 \n y 2 \n z 3" ?=> ["x", "1", "y", "2", "z", "3"]
     ,"escaped1" ~: "x \\{ z" ?=> ["x", "\\{", "z"]
   ]
 where (?=>) str res = Right res ~=? parseList str
       fails str = (parseList str) `should_fail_` ()
       should_be str res = (B.unpack str) ~: Right res ~=? parseList str

parseTokensTests = "parseTokens" ~: TestList [
     " x " ~: "x" ?=> ([Word "x"], "")
     ," x y " ~: " x y " ?=> ([Word "x", Word "y"], " ")
     ,"x y" ~: "x y" ?=> ([Word "x", Word "y"], "")
     ,"x { y 0 }" ~: "x { y 0 }" ?=> ([Word "x", nosub " y 0 "], "")
     ,"x {y 0}" ~: "x {y 0}" ?=> ([Word "x", nosub "y 0"], "")
   ]
 where (?=>) str (res,r) = Right (res, r) ~=? parseTokens str
       nosub = mkNoSub
