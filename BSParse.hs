{-# LANGUAGE BangPatterns #-}
module BSParse ( runParse, doInterp, TclWord(..), dropWhite, parseList
            ,Result
            ,TokCmd
            ,bsParseTests
  ) where

import qualified Data.ByteString.Char8 as B
import Control.Monad
import Data.Ix
import Test.HUnit  -- IGNORE

data TclWord = Word !B.ByteString | Subcommand TokCmd | NoSub !B.ByteString Result | Expand TclWord deriving (Show,Eq)
type Result = Maybe ([TokCmd], BString)
type TokCmd = (TclWord, [TclWord])

type BString = B.ByteString

runParse :: B.ByteString -> Result
runParse s = multi (mainparse . dropWhite) s >>= \(wds, rem) -> return (asCmds wds, rem)

asCmds lst = [ let (c:a) = x in (c,a) | x <- lst, not (null x)]

mainparse str = if B.null str
                   then return ([], B.empty)
                   else do
                       h <- safeHead str
                       case h of
                        ';'  -> return ([], B.tail str)
                        '\n' -> return ([], B.tail str)
                        '#'  -> eatComment str
                        _    -> parseArgs str

parseArgs = multi (dispatch . dropWhite)

dispatch str = do h <- safeHead str
                  case h of
                   '{' -> (parseExpand `orElse` parseNoSub) str
                   '[' -> (parseSub `wrapWith` Subcommand) str
                   '"' -> parseStr str
                   '\\' -> handleEsc (B.drop 1 str)
                   _   -> wordToken str

handleEsc s = do h <- safeHead s
                 let rest = B.drop 1 s
                 case h of
                    '\n' -> (dispatch . dropWhite) rest
                    v    -> case wordTokenRaw rest of
                              Just (w, r) -> return (Word (B.concat [B.singleton '\\', B.singleton v, w]), r)
                              Nothing     -> return (Word (B.cons '\\' (B.singleton v)), rest)

parseList s = if onlyWhite s
               then return []
               else do (l,r) <- multi (listDisp . dWhite) $ s
                       guard (onlyWhite r)
                       return l
 where onlyWhite = B.all isWhite
       isWhite = (`elem` " \t\n")
       dWhite = B.dropWhile isWhite

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
                                  v  -> if v `B.elem` (B.pack "{}\" \t\n") then i else inner (i+1) False


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

doVarParse s = eatChar '$' s >>= parseVarRef

parseVarRef s = do 
         let flist = [ parseVarTerm `orElse` getNS
                      ,tryGet parseVarRef
                      ,tryGet parseInd]
         chain flist s

getNS = chain [parseLit (B.pack "::"), parseVarTerm, tryGet getNS]

parseVarTerm = getvar `orElse` brackVar

oneOrMore f s = do
  (s,r2) <- f s
  getrest r2 [s]
 where getrest r !acc = do
         case f r of
           Nothing -> return (B.concat (reverse acc), r)
           Just (s2,r2) -> getrest r2 (s2:acc)

parseInd str
  | B.null str || B.head str /= '(' = fail "no indexer"
  | otherwise                       = do ind <- B.elemIndex ')' str
                                         let (pre,post) = B.splitAt (ind+1) str
                                         return (pre, post)

orElse a b = \v -> (a v) `mplus` (b v)
{-# INLINE orElse #-}

chain lst rs = inner lst [] rs
 where inner [] !acc !r     = return (B.concat (reverse acc), r)
       inner (f:fs) !acc !r = do (s,r2) <- f r 
                                 inner fs (s:acc) r2
 

parseLit !w s = do 
      let wlen = B.length w
      let slen = B.length s
      if wlen <= slen && w == B.take wlen s
         then return (w, B.drop wlen s)
         else fail "didn't match"

eatChar c s = parseChar c s >>= return . snd
{-# INLINE eatChar #-}

parseChar !c s = do
  if B.null s || B.head s /= c 
         then fail $ "didn't match, expected " ++ show c
         else return (B.splitAt 1 s)
{-# INLINE parseChar #-}

multi p s = do (w,r) <- p s
               if B.null r
                 then return ([w],r)
                 else case multi p r of
                       Nothing -> return ([w],r)
                       Just (wx,r2) -> return $! (w:wx,r2)
{-# INLINE multi #-}

parseSub s = do 
      (p,r) <- eatChar '[' s >>= parseArgs
      aft <- eatChar ']' (dropWhite r)
      case p of
        [] -> fail "empty subcommand"
        (ph:pt) -> return ((ph,pt), aft)

eatComment = return . (,) [] . B.drop 1 . B.dropWhile (/= '\n')

dropWhite = B.dropWhile (\x -> x == ' ' || x == '\t')

{-
wordChar ' ' = False
wordChar !c = let ci = ord c in
  (ord 'a' <= ci  && ci <= ord 'z') || (ord 'A' <= ci  && ci <= ord 'Z') ||
  (ord '0' <= ci  && ci <= ord '9') || (c == '_')
wordChar !c = c /= ' ' && any (`inRange` c) [('a','z'),('A','Z'), ('0','9')]  || c == '_'
-}
wordChar !c = c /= ' ' && (inRange ('a','z') c || inRange ('A','Z') c || inRange ('0','9') c || c == '_')

parseWord s = getWord s >>= \(w,r) -> return (Word w, r)

getPred p s = if B.null w then fail "no match" else return $! (w,n)
 where (w,n) = B.span p s

getWord = getPred p
 where p c = wordChar c || (c `B.elem` (B.pack "+.-*()=/$:^%!&<>?"))

getvar = getPred wordChar

tryGet fn s = (fn `orElse` (\_ -> return (B.empty, s))) s

wrapWith fn wr s = fn s >>= \(!w,r) -> return (wr w, r) 
{-# INLINE wrapWith #-}

wordToken = wordTokenRaw `wrapWith` Word
wordTokenRaw  = (chain [parseChar '$', parseVar]) `orElse` getWord

parseVar s = do hv <- safeHead s
                case hv of
                  '{' -> brackVar s >>= \(w,r) -> return ((B.cons '{' (B.snoc w '}')), r)
                  _   -> chain [getWord, tryGet wordTokenRaw] s

brackVar x = eatChar '{' x >> nested x


parseStr = parseStrRaw `wrapWith` Word

parseStrRaw s = do 
  str <- eatChar '"' s
  loc <- B.elemIndex '"' str
  let (w,r) = B.splitAt loc str
  if escaped loc str then do (w1, v) <- parseStrRaw r
                             let nw =  B.snoc (B.take (B.length w - 1) w) '"'
                             return (B.append nw w1, v)
                     else return (w, B.tail r)

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

mkNoSub s = NoSub s (runParse s)

parseExpand s = do
  (_,r) <- parseLit (B.pack "{*}") s 
  rh <- safeHead r
  guard (not (rh `elem` " \n\t"))
  (dispatch `wrapWith` Expand) r

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
      "With esc" ~: (bp " \\} yeah! ", bp " ") ?=? "{ \\} yeah! } ",
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
    "global namespace" ~: (bp "", mkvar "::booga", bp "") ?=? "$::booga",
    ":::"              ~: noInterp "$:::booga",
    "some namespace" ~: (bp "", mkvar "log::booga", bp "") ?=? "$log::booga", -- TODO
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
    "basic arr2" ~: (bp " ", mkvar "boo(4)", bp " " ) ?=? " $boo(4) ",
    "basic arr3" ~: (bp "", mkvar "boo( 4,5 )", bp " " ) ?=? "$boo( 4,5 ) ",
    "Escaped []"   ~: noInterp "a \\[sub\\] thing.",
    "Lone $ works" ~: noInterp "a $ for the head of each rebel!",
    "Escaped lone $ works" ~: noInterp "a \\$ for the head of each rebel!",
    "unescaped $ after esc works" ~:
          (bp "a \\$", mkvar "variable", bp "") ?=? "a \\$$variable",
    "Escaped [] crazy" ~:
       (bp "a ",Right (mkwd "sub",[mklit "quail [puts 1]"]), bp " thing.") ?=? "a [sub \"quail [puts 1]\" ] thing."
  ]
 where noInterp str = Nothing ~=? getInterp (bp str)
       (?=?) res str = Just res ~=? getInterp (bp str)
       mkvar w = Left (B.pack w)

doInterpTests = TestList [
    "dollar escape"  ~: "oh $ yeah" ?!= "oh \\$ yeah",
    "brace escape"  ~: "oh [ yeah" ?!= "oh \\[ yeah",
    "tab escape"     ~: "a \t tab"  ?!= "a \\t tab",
    "slash escape"     ~: "slash \\\\ party"  ?!= "slash \\\\\\\\ party",
    "subcom"   ~: (bp "some ", Right (mkwd "cmd", []), bp "") ?=? "some [cmd]",
    "newline escape" ~: "\nline\n"  ?!= "\\nline\\n"
  ]
 where (?=?) res str = Right res ~=? doInterp (bp str)
       (?!=) res str = Left (bp res) ~=? doInterp (bp str)

getWordTests = TestList [
     "Simple" ~: badword "",
     "Simple2" ~: (mkwd "$whoa", bp "") ?=? "$whoa",
     "Simple with bang" ~: (mkwd "whoa!", bp " ") ?=? "whoa! "
  ]
 where badword str = Nothing ~=? parseWord (bp str)
       (?=?) res str = Just res ~=? parseWord(bp str)

nestedTests = TestList [
  "Fail nested" ~: Nothing ~=? nested (bp "  {       the end"),
  "Pass nested" ~: Just (bp "  { }", B.empty) ~=? nested (bp "{  { }}"),
  "Pass empty nested" ~: Just (bp " ", B.empty) ~=? nested (bp "{ }"),
  "Fail nested" ~: Nothing ~=? nested (bp "  { {  }"),
  "Pass escape" ~: "{ \\{ }" `should_be` " \\{ ",
  "Pass escape 2" ~: "{ \\{ \\{ }" `should_be` " \\{ \\{ ",
  "Pass escape 3" ~: "{ \\\\}" `should_be` " \\\\",
  "Pass escape 4" ~: "{ \\} }" `should_be` " \\} "
  ,"no bracks" ~: "happy" `should_fail` ()
 ]
 where should_be act exp = Just (bp exp, B.empty) ~=? nested (bp act)
       should_fail act () = Nothing ~=? nested (bp act)

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
     " x "     ~: " x "   ?=> [bp "x"]
     ,""       ~: ""      ?=> []
     ,"\t \t " ~: "\t \t" ?=> []
     ," x y "  ~: " x y " ?=> [bp "x", bp "y"]
     ,"x y" ~: "x y" ?=> [bp "x", bp "y"]
     ,"x { y 0 }" ~: "x { y 0 }" ?=> [bp "x", bp " y 0 "]
     ,"x [puts yay]" ~: "x [puts yay]" ?=> [bp "x", bp "[puts", bp "yay]"]
     ," y { \\{ \\{ \\{ } { x }" ~: " y { \\{ \\{ \\{ } { x }" ?=> [bp "y", bp " \\{ \\{ \\{ ", bp " x "]
     , "unmatched fail" ~: fails " { { "
     ,"x {y 0}" ~: "x {y 0}" ?=> [bp "x", bp "y 0"]
     ,"with nl" ~: "x  1 \n y 2 \n z 3" ?=> (map bp ["x", "1", "y", "2", "z", "3"])
     ,"escaped1" ~: "x \\{ z" ?=> (map bp ["x", "\\{", "z"])
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
     -- not yet
     -- ,"unquoted ws arr" ~: (pr ["puts","$arr(1 2)"]) ?=? "puts $arr(1 2)"
    ,"expand" ~: ([(mkwd "incr", [Expand (mkwd "$boo")])], "") ?=? "incr {*}$boo"
  ]
 where badword str = Nothing ~=? runParse (bp str)
       (?=?) (res,r) str = Just (res, bp r) ~=? runParse (bp str)
       pr (x:xs) = ([(mkwd x, map mkwd xs)], "")
       pr []     = error "bad test!"

bsParseTests = TestList [ nestedTests, testEscaped, brackVarTests,
                   parseStrTests, getInterpTests, getWordTests, doInterpTests,
                   parseArgsTests, parseListTests, runParseTests, parseVarRefTests]

-- # ENDTESTS # --
