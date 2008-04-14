module TclErr where

import qualified TclObj as T
import Control.Monad.Error

type RetVal = T.TclObj 
data Err = ERet !RetVal | EBreak 
          | EContinue | EDie String deriving (Eq,Show)

instance Error Err where
 noMsg    = EDie "An error occurred."
 strMsg s = EDie s


errCode e = case e of
    EDie _    -> 1
    ERet _    -> 2
    EBreak    -> 3
    EContinue -> 4
