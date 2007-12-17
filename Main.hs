module Main where

import System.IO
import System
import Hiccup
import Control.Monad
import qualified Data.ByteString.Char8 as B

main = do args <- getArgs 
          hSetBuffering stdout NoBuffering
          case args of
             [] -> mkInterp >>= runRepl
             [f] -> B.readFile f >>= runTcl >>= foll
 where foll (Left e) = putStrLn $ "error: "++ B.unpack e 
       foll _        = return ()
       runRepl i = do putStr "hiccup> "
                      eof <- isEOF
                      when (not eof) $ do
                         ln <- B.getLine
                         unless (B.null ln) $ do
                             v <- runInterp i ln 
                             case v of
                                Left e -> putStrLn $ "error: " ++ B.unpack e
                                Right o -> unless (B.null o) (B.putStrLn o)
                         runRepl i
