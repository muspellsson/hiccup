import Control.Concurrent
import Control.Monad
import System.Posix.Files
import System.Posix.Signals ()
import System
import qualified Control.Exception as E

main = do
  files <- getArgs
  E.catch (watchLoop 0 files) print

sleep n = threadDelay ((n * 1000000))

watchLoop latest files = do
  nlatest <- latestMod files
  if nlatest > latest
    then do
      when (latest > 0) $ print (nlatest - latest)
      putStrLn $ "Files: " ++ show files
      watchLoop nlatest files
    else do sleep 3
            watchLoop latest files
   
latestMod fnl = mapM modTime fnl >>= return . maximum

modTime fn = getFileStatus fn >>= return . modificationTime
