module Main where

import System.IO
import System
import Hiccup
import qualified TclObj as T
import Extend
import Control.Monad
import System.Console.Editline.Readline
import qualified Data.ByteString.Char8 as B

main = do args <- getArgs 
          hSetBuffering stdout NoBuffering
          case args of
             []  -> mkMainInterp [] procs >>= runRepl
             (f:fs) -> do fdata <- B.readFile f 
                          res <- runTclWithArgs (map B.pack fs) procs fdata
                          res `unlessErr` (\_ -> return ())
 where unlessErr x f = either (\e -> B.putStrLn e) f x
       runRepl i = do mline <- readline "hiccup> "
                      case mline of
                        Nothing -> return ()
                        Just "" -> runRepl i
                        Just ln -> do addHistory ln 
                                      v <- interpEvalStr (B.pack ln) i
                                      v `unlessErr` (\o -> unless (B.null o) (B.putStrLn o))
                                      runRepl i


-- Example extension functions
procs = [tcl_sum, tcl_fib]
tcl_fib = tclProc "hfib" "n" fibw
 where fib n = if n <= 1 then 1 else fib (n-1) + fib (n-2)
       fibw ~[n] = do
          ni <- T.asInt n
          if ni <= 0 then tclErr "n must be positive integer"
                     else return . T.fromInt $ fib ni
      
tcl_sum = tclProc "hsum" "lst" hsum
 where hsum ~[lst] = do
          ints <- T.asList lst >>= mapM T.asInt
          return . T.fromInt $! sum ints
