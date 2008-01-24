module Util where
import qualified Data.ByteString.Char8 as B
import Control.Monad.Error
import Data.List(intersperse)
import Test.HUnit

type BString = B.ByteString

joinWith bsl c = B.concat (intersperse (B.singleton c) bsl)
{-# INLINE joinWith #-}
pack = B.pack
{-# INLINE pack #-}
unpack = B.unpack

mapSnd f = map (\(a,b) -> (a, f b))
mapFst f = map (\(a,b) -> (f a, b))

ifFails f v = f `orElse` (return v)

orElse f f2 = f `catchError` (\_ -> f2)

listEscape s = if (B.elem ' ' s && not hasBracks) || B.null s 
                 then B.concat [B.singleton '{', s, B.singleton '}'] 
                 else if hasBracks then B.concat (escapeStr s) else s
  where hasBracks = bdepth /= 0
        bdepth    = brackDepth s

brackDepth s = match 0 0 False
 where match i c esc = if i >= B.length s 
                          then c
                          else let ni = i+1  
                               in if esc then match ni c False 
                                         else case B.index s i of
                                               '{'  -> match ni (c+1) False
                                               '}'  -> match ni (c-1) False
                                               '\\' -> match ni c True
                                               _    -> match ni c False
                                       

escapeStr s = case B.findIndex (`elem` " \n\t{}") s of
                 Nothing -> [s]
                 Just i  -> let (b,a) = B.splitAt i s
                            in b : handle (B.head a) : escapeStr (B.drop 1 a)
 where handle '\n' = pack "\\n"
       handle ' '  = pack "\\ "
       handle '{'  = pack "\\{"
       handle '}'  = pack "\\}"
       handle _    = error "The impossible happened in handle"

utilTests = TestList [joinWithTests, listEscapeTests] where
 
  joinWithTests = TestList [
       ([""],'!') `joinsTo` "" 
       ,(["one"],'!') `joinsTo` "one" 
       ,(["one", "two"],',') `joinsTo` "one,two" 
    ]
   where joinsTo (a,s) r = pack r ~=? ((map pack a) `joinWith` s)

  listEscapeTests = TestList [
      noEscape "one" 
      ,noEscape "[andkda]" 
      ,"" `escapesAs` "{}"
      ,"a b" `escapesAs` "{a b}"
      ,"a { b" `escapesAs` "a\\ \\{\\ b"
      ,"a } b" `escapesAs` "a\\ \\}\\ b"
      ,"a {b} c" `escapesAs` "{a {b} c}"
      ," { " `escapesAs` "\\ \\{\\ "
    ] 
   where escapesAs a b = pack b ~=? listEscape (pack a)
         noEscape a = pack a ~=? listEscape (pack a)


