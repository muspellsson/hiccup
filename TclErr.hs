module TclErr where

import qualified TclObj as T
import Control.Monad.Error

type RetVal = T.TclObj 
data Err = ERet !RetVal | EBreak 
          | EContinue | EDie String deriving (Eq,Show)

instance Error Err where
 noMsg    = EDie "An error occurred."
 strMsg s = EDie s

e_OK, e_ERROR, e_RETURN, e_BREAK, e_CONTINUE :: Int
e_OK        = 0
e_ERROR     = 1
e_RETURN    = 2
e_BREAK     = 3
e_CONTINUE  = 4

errCode e = case e of
    EDie _    -> e_ERROR
    ERet _    -> e_RETURN
    EBreak    -> e_BREAK
    EContinue -> e_CONTINUE
