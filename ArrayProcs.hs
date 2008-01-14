module ArrayProcs (arrayProcs) where
import Common

import qualified TclObj as T
import qualified Data.ByteString.Char8 as B
import Control.Monad

arrayProcs = makeProcMap $
  [("array", procArray)]


procArray args = case map T.asStr args of
                  ["size", n] -> undefined
                  _ -> tclErr "bad array command"
