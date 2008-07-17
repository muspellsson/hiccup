{-# LANGUAGE BangPatterns,OverloadedStrings #-}
module BSParse where

import qualified Data.ByteString.Char8 as B
import Control.Monad (mplus)
import Data.Ix
import Util 
import Test.HUnit 

type Parser a = BString -> Result a
type ParseMonad = Either String
type Result a = ParseMonad (a, BString)

eatSpaces s = return ((), dropSpaces s)
{-# INLINE eatSpaces #-}

emit v s = return (v,s)
{-# INLINE emit #-}

consumed :: Parser t -> Parser BString
consumed p s = do 
    (_,r) <- p s 
    let lendiff = B.length s - B.length r
    return (B.take lendiff s, r)
{-# INLINE consumed #-}

orElse :: Parser t -> Parser t -> Parser t
orElse a b = \v -> v `seq` ((a v) `mplus` (b v))
{-# INLINE orElse #-}

tryGet fn = fn `orElse` (emit "")
     
chain_ lst = consumed (foldr (.>>) (emit ()) lst)
chain :: [Parser BString] -> Parser BString
chain lst = (foldr pcons (emit []) lst) `wrapWith` B.concat
{-# INLINE chain #-}

choose = foldr1 orElse
{-# INLINE choose #-}

pjoin :: (t1 -> t2 -> t3) -> Parser t1 -> Parser t2 -> Parser t3
pjoin op a b !s = do
       (w,r)   <- a s
       (w2,r2) <- b r
       return ((op w w2), r2)
{-# INLINE pjoin #-}

pass = pjoin (\a _ -> a)
{-# INLINE pass #-}

(.>>) = pjoin (\_ b -> b)
{-# INLINE (.>>) #-}

pcons :: Parser t -> Parser [t] -> Parser [t]
pcons = pjoin (:)
{-# INLINE pcons #-}

parseMany :: Parser t -> Parser [t]
parseMany p = inner `orElse` (emit [])
 where inner = parseMany1 p
   
parseMany1 p = p `pcons` (parseMany p)
{-# INLINE parseMany1 #-}

-- TODO: Document and possibly rename 'multi1'
multi1 :: Parser t -> Parser [t]
multi1 p = p `pcons` (parseEof `orElse` (multi1 p))
{-# INLINE multi1 #-}

 
parseLit :: BString -> Parser BString
parseLit !w s = if w `B.isPrefixOf` s 
                    then return (w, B.drop (B.length w) s) 
                    else fail $ "didn't match " ++ show w
{-# INLINE parseLit #-}


parseLen :: Int -> Parser BString
parseLen !i = \s -> if B.length s < i 
                     then fail ("can't parse " ++ show i ++ ", not enough")
                     else return $ B.splitAt i s
{-# INLINE parseLen #-}
         
parseOneOf !cl = parseCharPred (`elem` cl) ("one of " ++ show cl)

pchar :: Char -> Parser BString
pchar !c = parseCharPred (== c) (show c)
{-# INLINE pchar #-}

parseAny = parseCharPred (const True) "any char"
parseCharPred pred exp s = case B.uncons s of
                            Nothing    -> failStr "eof"
                            Just (h,t) -> if pred h then return (B.singleton h,t)
                                                    else failStr (show h)
 where failStr what = fail $ "expected " ++ exp ++ ", got " ++ what
{-# INLINE parseCharPred #-}


wrapWith fn wr s = fn s >>= \(!w,r) -> return (wr w, r) 
{-# INLINE wrapWith #-}

getPred p s = return $! (w,n)
 where (w,n) = B.span p s

-- TODO: slightly inaccuate error messages
getPred1 p desc s = if B.null w then fail ("wanted " ++ desc ++ ", got eof") else return $! (w,n)
 where (w,n) = B.span p s

parseEof s = if B.null s 
               then return ([], s) 
               else fail $ "expected eof, got " ++ show (B.head s)

sepBy1 :: Parser t -> Parser t2 -> Parser [t]
sepBy1 p sep = p `pcons` (parseMany (sep .>> p))


commaSep :: Parser t -> Parser [t]
commaSep p = (p `sepBy1` (eatSpaces .>> pchar ',')) `orElse` (emit [])


safeHead r s = if B.null s then fail ("expected " ++ r ++ ", got eof") else return (B.head s)
{-# INLINE safeHead #-}

brackets p = (pchar '[' .>> p) `pass` (eatSpaces .>> pchar ']')

wordChar !c = c /= ' ' && (inRange ('a','z') c || inRange ('A','Z') c || inRange ('0','9') c || c == '_')

braceVar = parseBlock

parseStr = pchar '"' .>> (inside `wrapWith` B.concat) `pass` (pchar '"')
 where noquotes = getPred1 (`notElem` "\"\\") "non-quote chars"
       inside = parseMany (noquotes `orElse` escapedChar)


escapedChar = chain [pchar '\\', parseAny]

parseBlock = pchar '{' .>> nest_filling `pass` pchar '}'
 where inner = choose [escapedChar, braces, nobraces]
       nest_filling = tryGet ((parseMany inner) `wrapWith` B.concat)
       braces = chain [pchar '{', nest_filling, pchar '}']
       nobraces = getPred1 (`notElem` "{}\\") "non-brace chars"
{-# INLINE parseBlock #-}



-- # TESTS # --

should_fail_ act _ = let res = case act of 
                                 Left _ -> True
                                 _      -> False
                     in TestCase $ assertBool "should fail" res

parseStrTests = "parseStr" ~: TestList [
      "Escaped works" ~: ("Oh \\\"yeah\\\" baby.", "") ?=? "\"Oh \\\"yeah\\\" baby.\"",
      "Parse Str with leftover" ~: ("Hey there.", " 44") ?=? "\"Hey there.\" 44",
      "Parse Str with dolla" ~: ("How about \\$44?", "") ?=? "\"How about \\$44?\"",
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


blockTests = "code block" ~: TestList [
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
 where should_be act exp = Right (exp, B.empty) ~=? parseBlock act
       should_fail act _ = (parseBlock act) `should_fail_` ()


bsParseTests = TestList [ blockTests, braceVarTests, parseStrTests ]

-- # ENDTESTS # --
