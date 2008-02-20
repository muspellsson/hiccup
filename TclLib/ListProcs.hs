module TclLib.ListProcs (listProcs,procList) where
import Common
import Util
import qualified TclObj as T
import TclObj ((.==))
import Control.Monad
import qualified Data.Sequence as S
import Data.Sequence ((><))

listProcs = makeProcMap $
  [("list", procList),("lindex",procLindex),
   ("llength",procLlength), ("lappend", procLappend), 
   ("lset", procLset), ("lassign", procLassign)]

procList = return . T.mkTclList

procLindex args = case args of
          [l]   -> return l
          [l,i] -> do items <- T.asSeq l
                      if T.isEmpty i 
                           then return l 
                           else do
                            ind   <- toInd items i
                            if ind >= S.length items || ind < 0 then ret else return (S.index items ind)
          _     -> argErr "lindex"
 where toInd s i = T.asInt i `orElse` tryEnd s i
       tryEnd s i = if i .== "end" then return ((S.length s) - 1) else return (-1)

procLlength args = case args of
        [lst] -> T.asSeq lst >>= return . T.mkTclInt . S.length
        _     -> argErr "llength"

procLset args = case args of
        [name,val] -> varModify (T.asBStr name) (\_ -> return val)
        [name,ind,val] ->  varModify (T.asBStr name) $
                           \old -> do
                               items <- T.asSeq old
                               if T.isEmpty ind 
                                  then return $! val
                                  else do
                                      i <- T.asInt ind
                                      rangeCheck items i
                                      return $! T.mkTclList' (S.update i val items)
        _              -> argErr "lset"
 where rangeCheck seq i = if i < 0 || i >= (S.length seq) then tclErr "list index out of range" else return ()
                              
procLassign args = case args of
  (list:(varnames@(_:_))) -> do l <- T.asList list
                                let (src,rest) = splitAt (length varnames) l
                                zipWithM_ setter varnames (src ++ repeat T.empty)
                                return (T.mkTclList rest)
  _ -> argErr "lassign"
 where setter n v = varSet (T.asBStr n) v


procLappend args = case args of
        (n:news) -> varModify (T.asBStr n)  $
                \old -> do items <- T.asSeq old
                           return $ T.mkTclList' (items >< (S.fromList news))
        _        -> argErr "lappend"

