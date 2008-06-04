module TclLib.ListProcs (listCmds) where
import Common
import Util
import Match (globMatch)
import Data.List (sortBy)
import Data.Ord (comparing)
import Proc.Util (mkLambda)
import TclLib.LibUtil
import qualified TclObj as T
import Control.Monad
import qualified Data.Sequence as S
import Data.Sequence ((><))

listCmds = makeCmdList $
  [("list", cmdList),("lindex",cmdLindex),
   ("llength",cmdLlength), ("lappend", cmdLappend), ("lsearch", cmdLsearch),
   ("lset", cmdLset), ("lassign", cmdLassign), ("lsort", cmdLsort),
   ("lrange", cmdLrange), ("lmap", cmdLmap),
   ("join", cmdJoin), ("concat", cmdConcat), ("lrepeat", cmdLrepeat)]

cmdList = return . T.fromList

cmdLindex args = case args of
          [l]   -> return l
          [l,i] -> if T.isEmpty i then return l
                      else do
                        items <- T.asSeq l
                        let ilen = S.length items                            
                        ind <- toIndex ilen i
                        if ind >= ilen || ind < 0 then ret else return (S.index items ind)
          _     -> argErr "lindex"

cmdLlength args = case args of
        [lst] -> T.asSeq lst >>= return . T.fromInt . S.length
        _     -> vArgErr "llength list"

cmdLset args = case args of
        [name,val] -> modifyVar (T.asVarName name) (\_ -> return val)
        [name,ind,val] -> modifyVar (T.asVarName name) $
                           \old -> do
                               items <- T.asSeq old
                               if T.isEmpty ind 
                                  then return $! val
                                  else do
                                      i <- toIndex (S.length items) ind
                                      rangeCheck items i
                                      return $! T.fromSeq (S.update i val items)
        _              -> argErr "lset"
 where rangeCheck seq i = when (i < 0 || i >= (S.length seq)) $ fail "list index out of range" 
       modifyVar vn f = do
            o <- varGetNS vn
            n <- f o
            varSetNS vn $! n
                              
cmdLassign args = case args of
  (list:(varnames@(_:_))) -> do l <- T.asList list
                                let (src,rest) = splitAt (length varnames) l
                                zipWithM_ setter varnames (src ++ repeat T.empty)
                                return (T.fromList rest)
  _ -> argErr "lassign"
 where setter n v = varSetNS (T.asVarName n) v

cmdJoin args = case args of
   [lst]     -> dojoin lst (pack " ")
   [lst,sep] -> dojoin lst (T.asBStr sep)
   _         -> argErr "join"
 where dojoin ll sep = do
         lst <- T.asList ll
         return $ T.fromBStr (joinWithBS (map T.asBStr lst) sep)

cmdConcat = return . T.objconcat

cmdLappend args = case args of
        (n:news) -> do 
             let vn = T.asVarName n 
             items <- varGetNS vn >>= T.asSeq 
             varSetNS vn $ T.fromSeq (items >< (S.fromList news))
        _        -> argErr "lappend"

cmdLsearch args = case args of
       [lsto,pat] -> do 
              let p = T.asBStr pat
              ilst <- liftM (zip [0..]) (T.asList lsto)
              return $ T.fromInt $ findIt p ilst
       _         -> argErr "lsearch"
 where findIt _ [] = -1
       findIt p ((i,e):xs) = if globMatch p (T.asBStr e) then i else findIt p xs

data SortType = AsciiSort | IntSort deriving (Eq,Show)
data SortFlags = SF { sortType :: SortType, sortReverse :: Bool, noCase :: Bool } deriving (Eq, Show)

accumFlags [] sf = return sf
accumFlags (x:xs) sf = case T.asStr x of
              "-ascii"      -> accumFlags xs (sf { sortType = AsciiSort })
              "-integer"    -> accumFlags xs (sf { sortType = IntSort })
              "-decreasing" -> accumFlags xs (sf { sortReverse = True })
              "-increasing" -> accumFlags xs (sf { sortReverse = False })
              "-nocase"     -> accumFlags xs (sf { noCase = True })
              unrecognized  -> tclErr $ "unrecognized lsort option: " ++ unrecognized
                        
defaultSort = SF { sortType = AsciiSort, sortReverse = False, noCase = False }

cmdLsort args = case args of
          []    -> argErr "lsort"
          alst  -> let (opts,lst) = (init alst, last alst)
                   in do sf <- accumFlags opts defaultSort
                         dosort sf lst
 where dosort sf lst = do
              items <- T.asList lst 
              sortEm sf items >>= return . T.fromList


-- TODO: This is so ugly.
sortEm :: SortFlags -> [T.TclObj] -> TclM [T.TclObj]
sortEm (SF stype rev nocase) lst = do 
     pairs <- modder
     return $ post pairs
  where paired f = mapM (\x -> f x >>= \nx -> return (nx, x)) lst
        caser = downCase `ifTrue` nocase
        retSortFst :: (Ord a) => [(a,T.TclObj)] -> TclM [T.TclObj]
        retSortFst = return . map snd . sortBy (comparing fst)
        post = reverse `ifTrue` rev
        modder = case stype of
                  AsciiSort -> paired (return . caser . T.asBStr) >>= retSortFst
                  IntSort   -> paired (T.asInt) >>= retSortFst
        ifTrue fun b = if b then fun else id

cmdLrepeat args = case args of 
    (ti:x:xs) -> do 
          i <- T.asInt ti 
          when (i <= 0) $ fail "must have a count of at least 1"
          return $ T.fromList (concat (replicate i (x:xs)))
    _ -> argErr "lrepeat"

cmdLrange args = case args of
   [lst,beg,e] -> do 
          items <- T.asSeq lst
          let ilen = S.length items
          i1 <- toIndex ilen beg
          i2 <- toIndex ilen e
          return $ T.fromSeq (S.take (i2 - i1 + 1) (S.drop i1 items))
   _           -> argErr "lrange"

cmdLmap args = case args of
  [fun,lst] -> do
         fn <- mkLambda fun 
         T.asList lst >>= mapM (fn . box) >>= return . T.fromList 
  _          -> argErr "map"
 where box i = [i]   
