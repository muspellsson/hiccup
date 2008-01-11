module ListProcs (listProcs,procList) where
import Common
import qualified TclObj as T
import qualified Data.ByteString.Char8 as B
import Control.Monad

listProcs = makeProcMap $
  [("list", procList),("lindex",procLindex),
   ("llength",procLlength)]

onObj f o = (f (T.asBStr o))

procList, procLindex, procLlength :: TclProc
procList a = treturn $ (map (escape . T.asBStr) a) `joinWith` ' '
 where escape s = if B.elem ' ' s then B.concat [B.singleton '{', s, B.singleton '}'] else s

procLindex args = case args of
          [l]   -> return l
          [l,i] -> do items <- T.asList l
                      ind   <- T.asInt i
                      if ind >= length items then ret else treturn (items !! ind)
          _     -> argErr "lindex"

procLlength args = case args of
        [lst] -> if B.null `onObj` lst 
                        then return T.tclFalse
                        else liftM (T.mkTclInt . length) (T.asList lst) 
        _     -> argErr "llength"

