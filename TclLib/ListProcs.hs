module TclLib.ListProcs (listProcs,procList) where
import Common
import qualified TclObj as T
import Control.Monad
import qualified Data.Sequence as S
import Data.Sequence ((><))

listProcs = makeProcMap $
  [("list", procList),("lindex",procLindex),
   ("llength",procLlength), ("lappend", procLappend), ("lset", procLset)]

procList = return . T.mkTclList

procLindex args = case args of
          [l]   -> return l
          [l,i] -> do items <- T.asSeq l
                      ind   <- T.asInt i
                      if ind >= S.length items then ret else return (S.index items ind)
          _     -> argErr "lindex"

procLlength args = case args of
        [lst] -> T.asSeq lst >>= return . T.mkTclInt . S.length
        _     -> argErr "llength"

procLset args = case args of
        [name,ind,val] ->  varModify (T.asBStr name) $
                           \old -> do
                               items <- T.asSeq old
                               i <- T.asInt ind
                               return $! T.mkTclList' (S.update i val items)
        _              -> argErr "lset"
                              


procLappend args = case args of
        (n:news) -> varModify (T.asBStr n)  $
                \old -> do items <- T.asSeq old
                           return $ T.mkTclList' (items >< (S.fromList news))
        _        -> argErr "lappend"

