module ArrayProcs (arrayProcs) where
import Common

import qualified TclObj as T
import qualified Data.ByteString.Char8 as B
import Control.Monad
import qualified Data.Map as Map

arrayProcs = makeProcMap $
  [("array", procArray)]


procArray :: TclProc
procArray args = case map T.asStr args of
                  ["size", n] -> do arr <- getArray (B.pack n) `ifFails` Map.empty
                                    return . T.mkTclInt $ Map.size arr
                  [n] -> tclErr $ "bad option " ++ show n
                  []  -> argErr "array"
