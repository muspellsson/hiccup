module TclChan ( TclChan(..), emptyChanMap, mkChan, ChanMap, insertChan, lookupChan, deleteChan, baseChans ) where

import Data.Unique
import Util

import System.IO
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map

newtype ChanMap = CM (Map.Map BString TclChan)

emptyChanMap = CM Map.empty

insertChan c (CM m) = CM (Map.insert (chanName c) c m)
lookupChan n (CM m) = Map.lookup n m
deleteChan c (CM m) = CM (Map.delete (chanName c) m)
namesChan (CM m) = Map.keys m

data TclChan = TclChan { chanHandle :: Handle, chanName :: BString } deriving (Eq,Show)

tclStdChans = [ mkChan' stdout "stdout", mkChan' stdin "stdin", mkChan' stderr "stderr" ]


mkChan h = do n <- uniqueNum
              return (mkChan' h ("file" ++ show n))
 where uniqueNum = newUnique >>= return . hashUnique

mkChan' h n = TclChan h (BS.pack n)
baseChans = CM (Map.fromList (map (\c -> (chanName c, c)) tclStdChans ))
