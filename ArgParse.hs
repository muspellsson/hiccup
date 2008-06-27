{-# LANGUAGE BangPatterns #-}
module ArgParse ( 
     mkArgSpecs, 
     parseArgs, 
     ArgSpec(..),
     boolFlagSpec,
     argParseTests ) where

import qualified Data.Map as Map
import qualified TclObj as T
import qualified Data.ByteString.Char8 as B
import Util (commaList)

import Test.HUnit

data ArgSpec t a = NoArg String (a -> a) | OneArg String (t -> a -> a)

argLabel (NoArg s _) = s
argLabel (OneArg s _) = s

mkArgSpecs keep al = (Map.fromList (map pairup al), keep)
 where pairup x = (B.pack (argLabel x), x)


choiceList m = commaList "or" (map (('-':) . B.unpack) (reverse (Map.keys m)))

boolFlagSpec name keep = mkArgSpecs keep [NoArg name (const True)]

parseArgs (as,keep) i al = inner (length al) al i
 where badOpt n = fail $ "bad option " ++ show (T.asBStr n) ++ ": must be " ++ choiceList as
       inner  _ []        !acc = return (acc,[])
       inner !r xl@(x:xs) !acc
         | r <= keep = return (acc,xl)
         | otherwise = do
            let badopt = badOpt x
            case B.uncons (T.asBStr x) of 
                Just ('-',rs) -> 
                       case Map.lookup rs as of
                           Nothing -> badopt
                           Just (NoArg _ f)  -> inner (r-1) xs (f acc)
                           Just (OneArg n f) -> 
                               case xs of
                                  (b:xxs) -> inner (r-2) xxs (f b acc)
                                  _  -> fail $ "flag requires argument: -" ++ n
                _     -> badopt

argParseTests = TestList [
    "boolFlagSpec" ~: boolFlagTests
  ]

boolFlagTests = TestList [ 
    (noCase,False,"candy shop") `should_be` (False, "candy shop") 
    ,(noCase,False,"-nocase candy shop") `should_be` (True, "candy shop") 
    ,(noCase,False,"shop") `should_be` (False, "shop") 
    ,(noCase,False,"-nocase candy") `should_be` (False, "-nocase candy") 
    ,(noCase,False,"nocase candy shop") `should_fail` ()
    ,(noCase,False,"-eatpie candy shop") `should_fail` ()
  ]
 where noCase = boolFlagSpec "nocase" 2
       should_be (spec,i,l) (r,al) = Right (r, sl al) ~=? (parseArgs spec i (sl l) :: Either String (Bool, [T.TclObj]))
       should_fail (spec,i,l) _ = Nothing ~=? parseArgs spec i (sl l)
       sl :: String -> [T.TclObj] 
       sl s = map T.fromStr (words s)
