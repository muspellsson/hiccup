{-# LANGUAGE BangPatterns,OverloadedStrings #-}
module TclLib.IOCmds (ioCmds, ioInits) where
import Common
import Control.Monad (unless, liftM2)
import System.IO
import System.Exit
import System.Directory (getCurrentDirectory,
                         doesFileExist, 
                         doesDirectoryExist,
                         getPermissions,
                         Permissions(..))
import System.Environment (getEnvironment)
import Core ()
import VarName (arrNameNS)
import qualified TclObj as T
import qualified TclChan as T
import qualified System.IO.Error as IOE 
import qualified Data.ByteString.Char8 as B
import TclObj ((.==))
import TclLib.LibUtil
import Util


setupEnv = do
  env <- io $ getEnvironment
  mapM_ (\(n,v) -> arrSet "env" (B.pack n) (T.fromStr v)) env
  return ()
 where arrSet n i v = varSetNS (arrNameNS n i) v

ioInits = [setupEnv]

ioCmds = nsCmdList "" $ safe ++ unsafe
 where safe = safeCmds [("puts",cmdPuts),("gets",cmdGets),("read",cmdRead),
                        ("close", cmdClose),("flush", cmdFlush),("eof", cmdEof),
                        ("tell", cmdTell)]
       unsafe = unsafeCmds [("exit", cmdExit), ("source", cmdSource),
                            ("pwd", cmdPwd), ("open", cmdOpen),("file", cmdFile)]

cmdEof args = case args of
  [ch] -> do h <- getReadable ch
             io (hIsEOF h) >>= return . T.fromBool 
  _    -> vArgErr "eof channelId"

cmdTell args = case args of
  [ch] -> do h <- lookupChan (T.asBStr ch) >>= return . T.chanHandle
             io (hTell h) >>= return . T.fromInt . fromIntegral
  _    -> vArgErr "tell channelId"

cmdRead args = case args of
     [ch] -> do
        h <- getReadable ch
        c <- io $ B.hGetContents h
        return $ c `seq` T.fromBStr c
     _ -> argErr "read"

cmdFile = mkEnsemble "file" [
    ("channels", file_channels),
    ("size", file_size),
    pred_check "exists" (\fn -> liftM2 (||) (doesFileExist fn)
                                            (doesDirectoryExist fn)),
    pred_check "isfile" doesFileExist,
    pred_check "isdirectory" doesDirectoryExist,
    permtest "readable" readable,
    permtest "executable" executable,
    permtest "writable" writable]
 where permtest n a = pred_check n (get_perm a)
       get_perm a fn = catch (getPermissions fn >>= return . a) (\_ -> return False)
       pred_check n p = 
        let cmd args = case args of
             [name] -> (io $ p (T.asStr name)) >>= return . T.fromBool
             _   -> vArgErr $ "file " ++ n ++ " name"
        in (n,cmd)
 
file_channels args = case args of
   [] -> namesChan >>= return . T.fromBList
   _  -> vArgErr "file channels"

file_size args = case args of
   [n] -> do 
      let name = T.asStr n 
      esiz <- io $ IOE.try (withFile name ReadMode hFileSize)
      case esiz of
        Left _ -> tclErr $ "could not read " ++ show name ++ ": no such file or directory"
        Right siz -> return . T.fromInt . fromIntegral $ siz
   _ -> vArgErr "file size name"

cmdPuts args = case args of
                 [s] -> (io . B.putStrLn . T.asBStr) s >> ret
                 [a1,str] -> if a1 .== "-nonewline" then tPut stdout str
                               else do h <- getWritable a1
                                       tPutLn h str
                 [a1,a2,str] ->do unless (a1 .== "-nonewline") bad
                                  h <- getWritable a2
                                  tPut h str 
                 _        -> bad
 where tPut = putFun B.hPutStr
       tPutLn = putFun B.hPutStrLn
       getWritable c = lookupChan (T.asBStr c) >>= checkWritable . T.chanHandle
       bad = vArgErr "puts ?-nonewline? ?channelId? string"
       putFun f h s = (io . f h . T.asBStr) s >> ret

cmdGets args = case args of
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
          _  -> vArgErr "gets channelId ?varName?"

getReadable c = lookupChan (T.asBStr c) >>= checkReadable . T.chanHandle

cmdSource args = case args of
                  [s] -> do 
                     let fn = T.asStr s 
                     dat <- useFile fn (slurpFile fn)
                     evalTcl (T.fromBStr dat :: T.TclObj)
                  _   -> vArgErr "source fileName"

cmdPwd args = case args of
  [] -> io getCurrentDirectory >>= return . T.fromStr
  _  -> vArgErr "pwd"

checkReadable c = do r <- io (hIsReadable c)
                     if r then return c else (tclErr "channel wasn't opened for reading")

checkWritable c = do r <- io (hIsWritable c)
                     if r then return c else (tclErr "channel wasn't opened for writing")

cmdOpen args = case args of
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

cmdClose args = case args of
         [ch] -> do h <- lookupChan (T.asBStr ch)
                    removeChan h
                    io (hClose (T.chanHandle h))
                    ret
         _    -> vArgErr "close channelId"

cmdFlush args = case args of
     [ch] -> do h <- lookupChan (T.asBStr ch)
                io (hFlush (T.chanHandle h))
                ret
     _    -> vArgErr "flush channelId"

cmdExit args = case args of
            [] -> io (exitWith ExitSuccess)
            [i] -> do v <- T.asInt i
                      let ecode = if v == 0 then ExitSuccess else ExitFailure v
                      io (exitWith ecode)
            _  -> vArgErr "exit ?returnCode?"


lookupChan :: BString -> TclM T.TclChan
lookupChan c = do chan <- getChan c
                  case chan of
                      Nothing -> tclErr ("cannot find channel named " ++ show c) 
                      Just ch -> return ch

