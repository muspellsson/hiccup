module Main where

import System.IO
import System
import Hiccup
import Control.Monad
import System.Console.Readline
import qualified Data.ByteString.Char8 as B

main = do args <- getArgs 
          hSetBuffering stdout NoBuffering
          case args of
             []  -> mkMainInterp >>= runRepl
             (f:fs) -> do fdata <- B.readFile f 
                          runTclWithArgs fdata (map B.pack fs) >>= (`unlessErr` (\_ -> return ()))
 where unlessErr x f = either (\e -> B.putStrLn e) f x
       runRepl i = do mline <- readline "hiccup> "
                      case mline of
                        Nothing -> return ()
                        Just "" -> runRepl i
                        Just ln -> do addHistory ln 
                                      v <- runInterp (B.pack ln) i
                                      v `unlessErr` (\o -> unless (B.null o) (B.putStrLn o))
                                      runRepl i
