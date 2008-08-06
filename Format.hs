{-# LANGUAGE OverloadedStrings #-}
module Format (formatString) where

import qualified Data.ByteString.Char8 as B
import qualified TclObj as T
import Data.Char (chr)
import BSParse (pchar,(.>>),choose,emit,wrapWith,parseMany,parseLit
                ,getPred1
                ,(</>)
                ,pass
                ,parseInt)

data Format = FInt | FStr | FChar deriving Show

getFormat f v = case f of
    FInt -> T.asInt v >>= return . B.pack . show
    FStr -> return . T.asBStr $ v
    FChar -> T.asInt v >>= return . B.singleton . chr


formatString str xl = case parseFormatStr str of
                   Right (fl,r) -> if B.null r then construct fl xl [] >>= return . B.concat
                                               else fail "invalid format string"
                   Left e       -> fail e

construct [] [] acc = return $ reverse acc
construct [] _ _ = fail "extra arguments to format"
construct ((Left s):xs) a acc = construct xs a (s:acc)
construct _  [] _ = fail "not enough arguments to format"
construct ((Right (p,f)):xs) (a:ax) acc = do
   s <- getFormat f a >>= return . pad p
   construct xs ax (s:acc)

pad n s = if plen > 0 then B.append (B.replicate plen ' ') s
                      else s
 where plen = n - B.length s

parseFormatStr = parseMany $ choose [normal `wrapWith` Left, 
                                     parseFormat `wrapWith` Right, 
                                     (parseLit "%%" .>> emit "%") `wrapWith` Left]
  where normal = getPred1 (/= '%') "non-format"

parseFormat = pchar '%' .>> choose [fstr,fchar,fint]
 where mk (c,v) = (padding `pass` pchar c) `wrapWith` (\p -> (p,v))
       fchar = mk ('c',FChar)
       fstr = mk ('s', FStr)
       fint = mk ('d',FInt)
       padding = parseInt </> emit 0
