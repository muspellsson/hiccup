module TclLib.ListProcs (listProcs,procList) where
import Common
import Util
import Match (globMatch)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.ByteString.Char8 (isPrefixOf)
import qualified TclObj as T
import Control.Monad
import qualified Data.Sequence as S
import Data.Sequence ((><))

listProcs = makeCmdList $
  [("list", procList),("lindex",cmdLindex),
   ("llength",procLlength), ("lappend", procLappend), ("lsearch", cmdLsearch),
   ("lset", procLset), ("lassign", procLassign), ("lsort", procLsort),
   ("lrange", cmdLrange),
   ("join", procJoin), ("concat", procConcat), ("lrepeat", cmdLrepeat)]

procList = return . T.mkTclList

cmdLindex args = case args of
          [l]   -> return l
          [l,i] -> do items <- T.asSeq l
                      if T.isEmpty i 
                           then return l 
                           else do
                            let ilen = S.length items                            
                            ind   <- toIndex ilen i
                            if ind >= ilen || ind < 0 then ret else return (S.index items ind)
          _     -> argErr "lindex"

procLlength args = case args of
        [lst] -> T.asSeq lst >>= return . T.fromInt . S.length
        _     -> argErr "llength"

procLset args = case args of
        [name,val] -> varModify (T.asVarName name) (\_ -> return val)
        [name,ind,val] ->  varModify (T.asVarName name) $
                           \old -> do
                               items <- T.asSeq old
                               if T.isEmpty ind 
                                  then return $! val
                                  else do
                                      i <- toIndex (S.length items) ind
                                      rangeCheck items i
                                      return $! T.mkTclList' (S.update i val items)
        _              -> argErr "lset"
 where rangeCheck seq i = when (i < 0 || i >= (S.length seq)) $ fail "list index out of range" 
                              
procLassign args = case args of
  (list:(varnames@(_:_))) -> do l <- T.asList list
                                let (src,rest) = splitAt (length varnames) l
                                zipWithM_ setter varnames (src ++ repeat T.empty)
                                return (T.mkTclList rest)
  _ -> argErr "lassign"
 where setter n v = varSetNS (T.asVarName n) v

procJoin args = case args of
   [lst]     -> dojoin lst (pack " ")
   [lst,sep] -> dojoin lst (T.asBStr sep)
   _         -> argErr "join"
 where dojoin ll sep = do
         lst <- T.asList ll
         return $ T.fromBStr (joinWithBS (map T.asBStr lst) sep)

procConcat = return . T.objconcat

procLappend args = case args of
        (n:news) -> varModify (T.asVarName n)  $
                \old -> do items <- T.asSeq old
                           return $ T.mkTclList' (items >< (S.fromList news))
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

procLsort args =  case args of
          []    -> argErr "lsort"
          alst  -> let (opts,lst) = (init alst, last alst)
                   in do sf <- accumFlags opts defaultSort
                         dosort sf lst
 where dosort sf lst = do
              items <- T.asList lst 
              sortEm sf items >>= return . T.mkTclList


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
          return $ T.mkTclList (concat (replicate i (x:xs)))
    _ -> argErr "lrepeat"

cmdLrange args = case args of
   [lst,beg,e] -> do 
          items <- T.asSeq lst
          let ilen = S.length items
          i1 <- toIndex ilen beg
          i2 <- toIndex ilen e
          return $ T.mkTclList' (S.take (i2 - i1 + 1) (S.drop i1 items))
   _           -> argErr "lrange"

toIndex len v = case T.asInt v of
                     Just i -> return i
                     Nothing -> let strval = T.asBStr v
                                in if strval `isPrefixOf` (pack "end") && not (bsNull strval)
                                     then return $ len - 1
                                     else fail "invalid index"
