module TclChan ( TclChan(..), mkChan, tclStdChans ) where

import Data.Unique

import TclObj
import System.IO
import qualified Data.ByteString.Char8 as BS


data TclChan = TclChan { chanHandle :: Handle, chanName :: BString } deriving (Eq,Show)

tclStdChans = [ mkChan' stdout "stdout", mkChan' stdin "stdin", mkChan' stderr "stderr" ]


mkChan h = do n <- uniqueNum
              return (mkChan' h ("file" ++ show n))
 where uniqueNum = newUnique >>= return . hashUnique

mkChan' h n = TclChan h (BS.pack n)
