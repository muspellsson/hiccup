module TclLib.IOProcs (ioCmds) where
import Common
import Control.Monad (unless)
import System.IO
import System.Exit
import Core (evalTcl)
import qualified TclObj as T
import qualified TclChan as T
import qualified System.IO.Error as IOE 
import qualified Data.ByteString.Char8 as B
import TclObj ((.==))
import TclLib.LibUtil
import Util

ioCmds = makeCmdList $ 
 [("puts",procPuts),("gets",procGets),("read",cmdRead),
  ("open", procOpen), ("close", procClose),("flush", procFlush),
  ("exit", procExit), ("source", procSource), ("eof", procEof)]

procEof args = case args of
  [ch] -> do h <- getReadable ch
             io (hIsEOF h) >>= return . T.fromBool 
  _    -> argErr "eof"

cmdRead args = case args of
     [ch] -> do
        h <- getReadable ch
        c <- io $ B.hGetContents h
        return $ c `seq` T.fromBStr c
     _ -> argErr "read"

procPuts args = case args of
                 [s] -> tPutLn stdout s
                 [a1,str] -> if a1 .== "-nonewline" then tPut stdout str
                               else do h <- getWritable a1
                                       tPutLn h str
                 [a1,a2,str] ->do unless (a1 .== "-nonewline") bad
                                  h <- getWritable a2
                                  tPut h str 
                 _        -> bad
 where tPut h s = (io . B.hPutStr h . T.asBStr) s >> ret
       tPutLn h s = (io . B.hPutStrLn h . T.asBStr) s >> ret
       getWritable c = lookupChan (T.asBStr c) >>= checkWritable . T.chanHandle
       bad = argErr "puts"

procGets args = case args of
          [ch] -> do h <- getReadable ch
                     eof <- io (hIsEOF h)
                     if eof then ret else (io . B.hGetLine) h >>= treturn
          [ch,vname] -> do h <- getReadable ch
                           eof <- io (hIsEOF h)
                           if eof
                             then varSetNS (T.asVarName vname) (T.empty) >> return (T.fromInt (-1))
                             else do s <- io (B.hGetLine h)
                                     varSetNS (T.asVarName vname) (T.fromBStr s)
                                     return $ T.fromInt (B.length s)
          _  -> argErr "gets"

getReadable c = lookupChan (T.asBStr c) >>= checkReadable . T.chanHandle

procSource args = case args of
                  [s] -> do 
                     let fn = T.asStr s 
                     dat <- useFile fn (slurpFile fn)
                     evalTcl (T.fromBStr dat :: T.TclObj)
                  _   -> argErr "source"

checkReadable c = do r <- io (hIsReadable c)
                     if r then return c else (tclErr "channel wasn't opened for reading")

checkWritable c = do r <- io (hIsWritable c)
                     if r then return c else (tclErr "channel wasn't opened for writing")

procOpen args = case args of
         [fn]   -> openChan fn ReadMode
         [fn,m] -> parseMode (T.asStr m) >>= openChan fn 
         _    -> argErr "open"
 where parseMode m =
         case m of
          "w" -> return WriteMode 
          "r" -> return ReadMode
          "a" -> return AppendMode
          _   -> fail "Unknown file mode"
       openChan fn m = do
        let name = T.asStr fn
        h <- useFile name (openFile name m)
        chan <- io (T.mkChan h)
        addChan chan
        treturn (T.chanName chan)

useFile fn fun = do
  eh <- io $ IOE.try fun
  case eh of
   Left e -> if IOE.isDoesNotExistError e 
	       then tclErr $ "could not open " ++ show fn ++ ": no such file or directory"
	       else tclErr (show e)
   Right h -> return h

procClose args = case args of
         [ch] -> do h <- lookupChan (T.asBStr ch)
                    removeChan h
                    io (hClose (T.chanHandle h))
                    ret
         _    -> argErr "close"

procFlush args = case args of
     [ch] -> do h <- lookupChan (T.asBStr ch)
                io (hFlush (T.chanHandle h))
                ret
     _    -> argErr "flush"

procExit args = case args of
            [] -> io (exitWith ExitSuccess)
            [i] -> do v <- T.asInt i
                      let ecode = if v == 0 then ExitSuccess else ExitFailure v
                      io (exitWith ecode)
            _  -> argErr "exit"


lookupChan :: BString -> TclM T.TclChan
lookupChan c = do chan <- getChan c
                  case chan of
                      Nothing -> tclErr ("cannot find channel named " ++ show c) 
                      Just ch -> return ch

