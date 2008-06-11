{-# LANGUAGE BangPatterns #-}
module ArgParse (mkArgSpecs, parseArgs, ArgSpec(..)) where

import qualified Data.Map as Map
import qualified TclObj as T
import qualified Data.ByteString.Char8 as B
import Util (commaList)

data ArgSpec t a = NoArg String (a -> a) | OneArg String (t -> a -> a)

argLabel (NoArg s _) = s
argLabel (OneArg s _) = s

mkArgSpecs keep al = (Map.fromList (map (\x -> (B.pack (argLabel x), x)) al),keep)
badList m = commaList "or" (map (('-':) . B.unpack) (reverse (Map.keys m)))

parseArgs (as,keep) i al = inner (length al) al i
 where badOpt n = fail $ "bad option " ++ show (T.asBStr n) ++ ": must be " ++ badList as
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
                _     -> if r /= keep 
                              then badopt
                              else return (acc,xl)
