{-# LANGUAGE BangPatterns #-}
module Match (match 
              ,MatchType(..)
              ,matchFun
              ,globMatch
              ,globMatches
              ,exactMatch
              ,exactMatches
              ,matchTests)  where

import qualified Data.ByteString.Char8 as B
import Data.Char (toLower)
import Test.HUnit

data MatchType = ExactMatch | GlobMatch deriving (Eq,Show)

matchFun ExactMatch = exactMatch
matchFun GlobMatch  = globMatch

globMatch pat = match False pat
exactMatch pat = (== pat)
globMatches pat = filter (globMatch pat)
exactMatches pat = filter (exactMatch pat) 

match :: Bool -> B.ByteString -> B.ByteString -> Bool
match nocase pat str = inner 0 0
 where slen = B.length str 
       plen = B.length pat
       ceq a b = if nocase then toLower a == toLower b else a == b
       inner !pi !si  
        | pi == plen = si == slen
        | otherwise = case B.index pat pi of
                       '*'  -> pi == (plen - 1) || or (map (inner (succ pi)) [si..(slen - 1)])
                       '?'  -> si /= slen && inner (succ pi) (succ si)
                       '['  -> let (npi, pred) = getSetPred pat pi plen 
                               in si /= slen && pred (B.index str si) && inner npi (succ si)
                       '\\' -> inner (succ pi) si
                       v    -> si /= slen && v `ceq` (B.index str si) && inner (succ pi) (succ si)

getSetPred pat pi plen = inner pi [] False
 where inner i lst rang = 
        if i == plen 
           then (i, (`elem` lst))
           else case (B.index pat i,rang) of
                 (']',False) -> (succ i, (`elem` lst)) 
                 (']',True) -> (succ i, (`elem` ('-':lst))) 
                 ('-',_) -> case lst of
                            [] -> inner (succ i) ['-'] False
                            _  -> inner (succ i) lst True
                 (c,False) -> inner (succ i) (c:lst) False
                 (c,True) -> let (h:tl) = lst 
                                 (a,z)  = minmax h c
                             in inner (succ i) ([a..z] ++ tl) False
       minmax a b = if b < a then (b,a) else (a,b)

matchTests = TestList [
    "boo" `matches` "boo" 
    ,"" `matches` "" 
    ,"1" `matches` "1" 
    ,"a?c" `matches` "abc" 
    ,"a?c" `doesnt_match` "ab" 
    ,"a??d" `matches` "abcd" 
    ,"f??d" `matches` "feed" 
    ,"b??n" `matches` "been"
    ,"a" `doesnt_match` "ab" 
    ,"ab" `doesnt_match` "a" 
    ,"a*" `matches` "abcde" 
    ,"[abcd]" `matches` "b"
    ,"[abcd]" `doesnt_match` ""
    ,"a[bcd" `matches` "ac"
    ,"[a-z]" `matches` "b"
    ,"[z-a]" `matches` "b"
    ,"[abcd]*" `matches` "abdc"
    ,"a*b" `matches` "ab" 
    ,"a*b" `matches` "a OMG b" 
    ,"*b" `matches` "in the land of bob" 
    ,"*" `matches` "in the land of bob" 
    ,"*" `matches` ""
    ,"s\\*r" `matches` "s*r"
  ]
 where matches a b = (show a) ++ " matches " ++ (show b) ~: True ~=? match False (B.pack a) (B.pack b) 
       doesnt_match a b = (show a) ++ " doesn't match " ++ (show b) ~: False ~=? match False (B.pack a) (B.pack b)
