module TclErr where

import qualified TclObj as T
import Control.Monad.Error

type RetVal = T.TclObj
data Err = Err !Int (Maybe T.TclObj) | ErrTramp Err deriving (Eq,Show)

instance Error Err where
 noMsg    = eDie "An error occurred."
 strMsg s = eDie s

eDie s = Err e_ERROR (Just (T.fromStr s))

errData (Err _ (Just v)) = v
errData _                = T.empty

errCode (Err i _) = i
errCode (ErrTramp _) = e_RETURN

fromCode ec = Err (fromEnum ec) Nothing
data ErrCode = EOk | EError | EReturn | EBreak | EContinue deriving (Enum,Eq,Show)

e_OK, e_ERROR, e_RETURN, e_BREAK, e_CONTINUE :: Int
e_OK        = 0
e_ERROR     = 1
e_RETURN    = 2
e_BREAK     = 3
e_CONTINUE  = 4

