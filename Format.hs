module Format (formatString) where

import qualified Data.ByteString.Char8 as B
import qualified TclObj as T
import Data.Char (chr)


formatString str xl = 
   case B.elemIndex '%' str of
     Nothing -> return str
     Just i -> let (pre,post) = B.splitAt i str
               in case B.uncons (B.drop 1 post) of
                    Nothing -> fail "bare %"
                    Just (c,r) -> handle pre c r
 where handle pre c r = 
         let withRest v l = formatString r l >>= \x -> return (B.concat [pre, v, x])
             argMod f = case xl of
                  (x:xs) -> f x >>= \v -> v `withRest` xs
                  []     -> fail "not enough args"
         in case c of
           '%' -> (B.singleton '%') `withRest` xl
           's' -> argMod (return . T.asBStr)
           'c' -> argMod (\v -> T.asInt v >>= return . B.singleton . chr)
           'd' -> argMod (\v -> T.asInt v >>= return . B.pack . show)
           _   -> fail $ "unknown format pattern: " ++ show c

                      
