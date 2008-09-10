module Extend (tclProc,tclErr, TclM, parseArgs) where

import Common (tclErr, TclM)
import Util
import qualified Data.ByteString.Char8 as B
import Internal.Types (TclCmd)
import qualified TclObj as T
import Proc.Params

tclProc :: String -> String -> TclCmd -> (String,TclCmd)
tclProc name args fun = (name,wfun)
 where (bsn, bsa) = (B.pack name, B.pack args) 
       wfun al = parseArgs bsn bsa al >>= fun

parseArgs :: BString -> BString -> [T.TclObj] -> TclM [T.TclObj]
parseArgs name pdef args = do
  params <- parseParams name (T.fromBStr pdef)
  bindArgs params args >>= return . map snd
