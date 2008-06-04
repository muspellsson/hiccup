{-# LANGUAGE BangPatterns #-}
module ArgParse (mkArgSpecs, parseArgs, ArgSpec(..)) where

import qualified Data.Map as Map
import qualified TclObj as T
import qualified Data.ByteString.Char8 as B

data ArgSpec t a = NoArg String (a -> a) | OneArg String (t -> a -> a)

argLabel (NoArg s _) = s
argLabel (OneArg s _) = s

mkArgSpecs al = Map.fromList (map (\x -> (B.pack (argLabel x), x)) al)

parseArgs as i al = inner al i
 where inner []     !acc = return (acc,[])
       inner (x:xs) !acc = 
        let badopt = fail $ "bad option " ++ show (T.asBStr x) in
        case B.uncons (T.asBStr x) of 
           Just ('-',rs) -> 
             case Map.lookup rs as of
               Nothing -> badopt
               Just (NoArg _ f)  -> inner xs (f acc)
               Just (OneArg n f) -> case xs of
                                     (b:xxs) -> inner xxs (f b acc)
                                     _  -> fail $ "flag requires argument: -" ++ n
           _     -> badopt
