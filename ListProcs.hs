module ListProcs (listProcs,procList) where
import Common
import qualified TclObj as T
import qualified Data.ByteString.Char8 as B
import Control.Monad

listProcs = makeProcMap $
  [("list", procList),("lindex",procLindex),
   ("llength",procLlength), ("lappend", procLappend)]

onObj f o = (f (T.asBStr o))

procList, procLindex, procLlength :: TclProc
procList = return . T.mkTclList . map T.asBStr

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

procLappend args = case args of
        (n:news) -> do old <- varGet (T.asBStr n) 
                       items <- T.asList old
                       let nl = T.mkTclList (items ++ (map T.asBStr news))
                       varSet (T.asBStr n) nl
                       return nl
        _        -> argErr "lappend"
                   
