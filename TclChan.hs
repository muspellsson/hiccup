module TclChan ( TclChan(..), mkChan, ChanMap, insertChan, lookupChan, deleteChan, baseChans ) where

import Data.Unique
import Util

import System.IO
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map

type ChanMap = Map.Map BString TclChan

insertChan c m = Map.insert (chanName c) c m
lookupChan n m = Map.lookup n m
deleteChan c m = Map.delete (chanName c) m

data TclChan = TclChan { chanHandle :: Handle, chanName :: BString } deriving (Eq,Show)

tclStdChans = [ mkChan' stdout "stdout", mkChan' stdin "stdin", mkChan' stderr "stderr" ]


mkChan h = do n <- uniqueNum
              return (mkChan' h ("file" ++ show n))
 where uniqueNum = newUnique >>= return . hashUnique

mkChan' h n = TclChan h (BS.pack n)
baseChans = Map.fromList (map (\c -> (chanName c, c)) tclStdChans )
