import Control.Concurrent
import Control.Monad
import System.Process
import System.Posix.Files
import Text.Printf
import System
import qualified Control.Exception as E

main = do
  files <- getArgs
  putStrLn $ "Watching " ++ unwords files
  E.catch (watcher files) print


watcher files = watchLoop 0 where  
 watchLoop latest = do
  nlatest <- latestMod files
  if nlatest > latest
    then do
      when (latest > 0) $ putStrLn $ show (nlatest - latest) ++ "s"
      evalCmd "sh ./test.sh"
      watchLoop nlatest 
    else sleep 3 >> watchLoop latest
   
latestMod fnl = mapM modTime fnl >>= return . maximum
modTime fn = getFileStatus fn >>= return . modificationTime
evalCmd c = runCommand c >>= waitForProcess
sleep n = threadDelay ((n * 1000000))
