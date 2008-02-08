{-# LANGUAGE BangPatterns #-}
module Common (RetVal, TclM
       ,TclState
       ,Err(..)
       ,TclProc,TclProcT(..)
       ,runTclM
       ,makeState
       ,runWithEnv
       ,withScope
       ,withNS
       ,makeProcMap
       ,getProc
       ,getProcNS
       ,regProc
       ,varGetNS
       ,varGet
       ,varModify
       ,varSet
       ,varSet'
       ,varExists
       ,varUnset
       ,varRename
       ,varDump
       ,varSetLocalVal
       ,getArray
       ,addChan
       ,removeChan
       ,getChan
       ,uplevel
       ,upvar
       ,makeEnsemble
       ,io
       ,tclErr
       ,treturn
       ,ret
       ,argErr
       ,stackLevel
       ,globalVars
       ,localVars
       ,currentVars
       ,currentNS
       ,parentNS
       ,existsNS
       ,childrenNS
       ,commandNames
       ,commonTests
    ) where

import qualified Data.ByteString.Char8 as B
import Control.Monad.Error
import qualified TclObj as T
import Control.Monad.State
import qualified Data.Map as Map
import TclChan
import Data.IORef
import Test.HUnit
import VarName
import Util


type RetVal = T.TclObj -- IGNORE

data Err = ERet !RetVal | EBreak | EContinue | EDie String deriving (Eq,Show)

instance Error Err where
 noMsg    = EDie "An error occurred."
 strMsg s = EDie s

type TclM = ErrorT Err (StateT TclState IO)

data Namespace = TclNS {
         nsName :: BString,
         nsFullName :: BString,
         nsProcs :: ProcMap,
         nsVars :: VarMap,
         nsParent :: Maybe (IORef Namespace),
         nsChildren :: Map.Map BString (IORef Namespace) } 

globalNS = TclNS { nsName = pack "::", nsFullName = pack "", nsProcs = emptyProcMap, nsVars = emptyVarMap, nsParent = Nothing, nsChildren = Map.empty }

data TclProcT = TclProcT { procName :: BString, procBody :: BString,  procFunction :: TclProc }

data TclFrame = TclFrame { frVars :: VarMap, upMap :: Map.Map BString (Int,BString) }
type TclStack = [TclFrame]
data TclState = TclState { tclChans :: ChanMap, tclStack :: TclStack, tclCurrNS :: IORef Namespace, tclGlobalNS :: IORef Namespace }
type TclProc = [T.TclObj] -> TclM RetVal
type ProcMap = Map.Map ProcKey TclProcT
type ProcKey = BString
type VarMap = Map.Map BString TclVar
data TclVar = ScalarVar T.TclObj | ArrayVar TclArray deriving (Eq,Show)

type TclArray = Map.Map BString T.TclObj

makeProcMap :: [(String,TclProc)] -> ProcMap
makeProcMap = Map.fromList . map toTclProcT . mapFst pack

toTclProcT (n,v) = (n, TclProcT n errStr v)
 where errStr = pack $ show n ++ " isn't a procedure"

makeState :: [(BString,T.TclObj)] -> ProcMap -> IO TclState
makeState = makeState' baseChans

makeState' :: ChanMap -> [(BString,T.TclObj)] -> ProcMap -> IO TclState
makeState' chans vlist procs = do let fr = emptyFrame { frVars = Map.fromList (mapSnd ScalarVar vlist) }
                                  ns <- newIORef (globalNS { nsProcs = procs })
                                  return (TclState chans [fr] ns ns)

stackLevel = getStack >>= return . pred . length
globalVars = upglobal localVars
localVars = getFrame >>= return . Map.keys . frVars
currentVars = do f <- getFrame
                 return $ Map.keys (frVars f) ++ Map.keys (upMap f)

commandNames = getProcMap >>= return . map procName . Map.elems

getStack = do s <- gets tclStack -- TODO: Guard for empty stack here?
              case s of
                []    -> tclErr "Aack. Tried to go up too far in the stack."
                v     -> return v
getProcMap = do currNs <- gets tclCurrNS >>= io . readIORef
                return (nsProcs currNs)
{-# INLINE getProcMap #-}
getGlobalProcMap = do gNs <- gets tclGlobalNS >>= io . readIORef
                      return (nsProcs gNs)

putProcMap p = do nsref <- gets tclCurrNS
                  io (modifyIORef nsref (\v -> v { nsProcs = p }))

putStack s = modify (\v -> v { tclStack = s })
modStack :: (TclStack -> TclStack) -> TclM ()
modStack f = getStack >>= putStack . f
getFrame = liftM head getStack

{-# INLINE getStack  #-}
{-# INLINE putStack  #-}
{-# INLINE getFrame  #-}

io :: IO a -> TclM a
io = liftIO

tclErr :: String -> TclM a
tclErr = throwError . EDie

argErr s = fail ("wrong # of args: " ++ s)

runTclM :: TclM a -> TclState -> IO (Either Err a, TclState)
runTclM code env = runStateT (runErrorT code) env

onChan f = gets tclChans >>= f
modChan f = modify (\s -> s { tclChans = f (tclChans s) })
getChan n = onChan (\m -> return (lookupChan n m))
addChan c    = modChan (insertChan c)
removeChan c = modChan (deleteChan c)

upped s e = Map.lookup s (upMap e)
{-# INLINE upped #-}

getProc pname = case parseNS pname of
    Left n -> getProcNorm n
    Right (nsl,n) -> getProcNS (NSRef (NS nsl) n)

getProcNS (NSRef Local k) = getProcNorm k
getProcNS (NSRef (NS nsl) k) = do
  nsref <- getNamespace nsl
  ns <- (io . readIORef) nsref
  return $ getProc' k (nsProcs ns)

getNamespace nsl = case nsl of
       (x:xs) -> do base <- if B.null x then gets tclGlobalNS else gets tclCurrNS >>= getKid x
                    getEm xs base
       []     -> fail "Something unexpected happened in getNamespace"
 where getEm []     ns = return ns
       getEm (x:xs) ns = getKid x ns >>= getEm xs
       getKid k nsref = do kids <- (io . readIORef) nsref >>= return . nsChildren
                           case Map.lookup k kids of
                               Nothing -> fail $ "can't find namespace " ++ show k
                               Just v  -> return v

existsNS ns = (getNamespace (explodeNS ns) >> return True) `catchError` (\_ -> return False)

getProcNorm :: ProcKey -> TclM (Maybe TclProcT)
getProcNorm i = do
  currpm <- getProcMap
  case getProc' i currpm of
    Nothing -> do globpm <- getGlobalProcMap
                  return (getProc' i globpm)
    x       -> return x

getProc' :: ProcKey -> ProcMap -> Maybe TclProcT
getProc' i m = Map.lookup i m

rmProc :: BString -> TclM ()
rmProc name = getProcMap >>= putProcMap . Map.delete name

regProc :: BString -> BString -> TclProc -> TclM RetVal
regProc name body pr = (getProcMap >>= putProcMap . pmInsert (TclProcT name body pr)) >> ret

pmInsert proc m = Map.insert (procName proc) proc m

varSet :: BString -> T.TclObj -> TclM RetVal
varSet !n v = varSetNS (parseVarName n) v

varSetNS (NSRef ns vn) v = runInNS ns (varSet' vn v)

varSetLocalVal :: BString -> T.TclObj -> TclM RetVal
varSetLocalVal n v = varSet' (VarName n Nothing) v
{-# INLINE varSetLocalVal #-}

varSet' :: VarName -> T.TclObj -> TclM RetVal
varSet' vn v = do
     (env:es) <- getStack
     case upped (vnName vn) env of
         Just (i,s) -> uplevel i (varSet' (vn {vnName = s}) v)
         Nothing    -> do ne <- modEnv env
                          putStack (ne:es)
                          return v
 where modEnv env = do
                let ev  = frVars env
                let str = vnName vn
                case vnInd vn of
                  Nothing -> case Map.lookup str ev of
                              Just (ArrayVar _) -> tclErr $ "can't set " ++ showVN vn ++ ": variable is array"
                              _                 -> return (env { frVars = Map.insert str (ScalarVar v) ev })
                  Just i  -> case Map.findWithDefault (ArrayVar Map.empty) str ev of
                               ScalarVar _     -> tclErr $ "Can't set " ++ showVN vn ++ ": variable isn't array"
                               ArrayVar prev ->  return (env { frVars = Map.insert str (ArrayVar (Map.insert i v prev)) ev })

varMod' :: VarName -> (T.TclObj -> TclM RetVal) -> TclM RetVal
varMod' vn@(VarName str ind) f = do
     (env:es) <- getStack
     case upped str env of
         Just (i,s) -> uplevel i (varMod' (vn { vnName = s }) f)
         Nothing    -> do (ne,v) <- modEnv env
                          putStack (ne:es)
                          return v
 where modEnv env = do
                let ev = frVars env
                let old = Map.lookup str ev
                case (old,ind) of
                  (Nothing,_) -> tclErr $ "no such variable: " ++ showVN vn
                  (Just os, Nothing) -> case os of
                                             ScalarVar val -> do
                                                    val2 <- f val
                                                    return ((env { frVars = Map.insert str (ScalarVar val2) ev }), val2)
                                             _ -> tclErr $ "can't read " ++ showVN vn ++ ", variable is array"
                  (Just oa, Just i)  -> case oa of
                                         ArrayVar prev -> do
                                             p2 <- Map.lookup i prev
                                             v <- f p2
                                             return ((env { frVars = Map.insert str (ArrayVar (Map.insert i v prev)) ev }), v)
                                         _ -> tclErr $ "can't set " ++ showVN vn ++ ": variable isn't array"

{-# INLINE varMod' #-}
varModify :: BString -> (T.TclObj -> TclM T.TclObj) -> TclM RetVal
varModify !n f = varModifyNS (parseVarName n) f
{-# INLINE varModify #-}

varModifyNS (NSRef ns vn) f = runInNS ns (varMod' vn f)

varExists :: BString -> TclM Bool
varExists name = (varGet name >> return True) `catchError` (\_ -> return False)

varRename :: BString -> BString -> TclM RetVal
varRename old new = do
  mpr <- getProc old
  case mpr of
   Nothing -> tclErr $ "bad command " ++ show old
   Just pr -> do rmProc old
                 if not (B.null new) then regProc new (procBody pr) (procFunction pr) else ret

varUnset :: BString -> TclM RetVal
varUnset name = do
  let (NSRef ns (VarName n _)) = parseVarName name
  runInNS ns (varUnset' n)

runInNS !ns f
  | isLocal ns  = f
  | isGlobal ns = upglobal f
  | otherwise   = do let (NS nsl) = ns
                     nsref <- getNamespace nsl
                     withExistingNS nsref f
{-# INLINE runInNS #-}

varUnset' :: BString -> TclM RetVal
varUnset' name = do
   (env:es) <- getStack
   case upped name env of
      Nothing    -> do let vmap = frVars env
                       verifyNameIn vmap
                       putStack ((env { frVars = Map.delete name vmap }):es)
      Just (i,s) -> do let umap = upMap env
                       verifyNameIn umap
                       putStack ((env { upMap = Map.delete name umap }):es)
                       uplevel i (varUnset' s) >> return ()
   ret
 where bad = tclErr ("can't unset " ++ show name ++ ": no such variable")
       verifyNameIn m = unless (Map.member name m) bad

varDump = do env <- getStack
             io (putStrLn "")
             io (mapM_ (\(i,a,b) -> (putStrLn (i ++ ":") >> putStrLn a >> putStrLn b))  (map fixit (zip [0..] env)))
  where fixit (i,s) = (show i, Map.showTree (frVars s), Map.showTree (upMap s))



getArray :: BString -> TclM TclArray
getArray name = do
   var <- varLookup name
   case var of
      Just (ArrayVar a) -> return a
      Just _            -> tclErr $ "can't read " ++ show name ++ ": variable isn't array"
      Nothing           -> tclErr $ "can't read " ++ show name ++ ": no such variable"

varLookup :: BString -> TclM (Maybe TclVar)
varLookup name = do
   env <- getFrame
   case upped name env of
      Nothing    -> return (Map.lookup name (frVars env))
      Just (i,n) -> uplevel i (varLookup n)

varGet :: BString -> TclM RetVal
varGet !n = varGetNS (parseVarName n)

varGetNS :: NSRef VarName -> TclM RetVal
varGetNS (NSRef ns vn) = runInNS ns (varGet' vn)

varGet' :: VarName -> TclM RetVal
varGet' vn@(VarName name ind) = do
  var <- varLookup name
  case var of
   Nothing -> cantReadErr "no such variable"
   Just o  -> o `withInd` ind
 where cantReadErr why  = tclErr $ "can't read " ++ showVN vn ++ ": " ++ why
       withInd (ScalarVar o) Nothing = return o
       withInd (ScalarVar _) _       = cantReadErr "variable isn't array"
       withInd (ArrayVar o) (Just i) = maybe (cantReadErr "no such element in array") return (Map.lookup i o)
       withInd (ArrayVar _)  _       = cantReadErr "variable is array"



uplevel :: Int -> TclM a -> TclM a
uplevel i p = do
  (curr,new) <- liftM (splitAt i) getStack
  putStack new
  res <- p `ensure` (modStack (curr ++))
  return res
{-# INLINE uplevel #-}

upglobal :: TclM a -> TclM a
upglobal p = do
  len <- stackLevel
  uplevel len p

upvar n d s = do (e:es) <- getStack
                 putStack ((e { upMap = Map.insert (T.asBStr s) (n, (T.asBStr d)) (upMap e) }):es)
                 ret
{-# INLINE upvar #-}

withScope :: TclM RetVal -> TclM RetVal
withScope f = withScope' emptyVarMap f >>= return . fst

withScope' :: VarMap -> TclM RetVal -> TclM (RetVal, TclFrame)
withScope' vm f = do
  (o:old) <- getStack
  putStack $ emptyFrame { frVars = vm } : o : old
  fun `ensure` (modStack (drop 1))
 where fun = do v <- f
                fr <- getFrame
                return (v,fr)

mkEmptyNS name parent = do
    pname <- liftM nsFullName (readIORef parent)
    new <- newIORef $ TclNS { nsName = name, nsFullName = B.concat [pname, pack "::", name],  nsProcs = emptyProcMap, nsVars = emptyVarMap, nsParent = Just parent, nsChildren = Map.empty }
    modifyIORef parent (\n -> n { nsChildren = Map.insert name new (nsChildren n) })
    return new

withNS :: BString -> TclM RetVal -> TclM RetVal
withNS name f = do
     newCurr <- getOrCreateNamespace name
     withExistingNS newCurr f

withExistingNS newCurr f = do
     nsref <- gets tclCurrNS
     putCurrNS newCurr
     (op newCurr) `ensure` (putCurrNS nsref)
 where op nsr = do vm <- getNSVars nsr 
                   (r,nsv) <- withScope' vm f
                   setNSVars nsr (frVars nsv)
                   return r

setNSVars nsref v = do
  ns <- (io . readIORef) nsref
  io $ writeIORef nsref (ns { nsVars = v })

getNSVars nsref = do
  ns <- (io . readIORef) nsref
  return (nsVars ns)



getOrCreateNamespace ns = case explodeNS ns of
       (x:xs) -> do base <- if B.null x then gets tclGlobalNS else gets tclCurrNS >>= getKid x
                    getEm xs base
       []     -> fail "Something unexpected happened in getOrCreateNamespace"
 where getEm []     ns = return ns
       getEm (x:xs) ns = getKid x ns >>= getEm xs
       getKid k nsref = do kids <- (io . readIORef) nsref >>= return . nsChildren
                           case Map.lookup k kids of
                               Nothing -> io (mkEmptyNS k nsref)
                               Just v  -> return v

putCurrNS ns = modify (\s -> s { tclCurrNS = ns })

currentNS = do
  ns <- gets tclCurrNS >>= io . readIORef
  return (nsFullName ns)

parentNS = do
 ns <- gets tclCurrNS >>= io . readIORef
 case nsParent ns of
   Nothing -> return B.empty
   Just v  -> (io . readIORef) v >>= return . nsFullName

childrenNS :: TclM [BString]
childrenNS = do
  ns <- gets tclCurrNS >>= io . readIORef
  (return . Map.keys . nsChildren) ns

ensure action p = do
   r <- action `catchError` (\e -> p >> throwError e)
   p
   return r

ret :: TclM RetVal
ret = return T.empty
{-# INLINE ret #-}

treturn :: BString -> TclM RetVal
treturn = return . T.mkTclBStr
{-# INLINE treturn #-}

emptyFrame = TclFrame { frVars = emptyVarMap, upMap = Map.empty }

makeEnsemble name subs = top
  where top args = case args of
                   (x:xs) -> case Map.lookup (T.asBStr x) subMap of
                              Nothing -> tclErr $ "unknown subcommand " ++ show (T.asBStr x) ++ ": must be " ++ commaList "or" (map unpack (procMapNames subMap))
                              Just f  -> (procFunction f) xs
                   []  -> argErr $ " should be \"" ++ name ++ "\" subcommand ?arg ...?"
        subMap = makeProcMap subs

procMapNames = map procName . Map.elems

emptyProcMap = Map.empty
emptyVarMap = Map.empty
-- # TESTS # --

runWithEnv :: TclM RetVal -> Either Err RetVal -> IO Bool
runWithEnv t v =
  do st <- makeState' Map.empty [] emptyProcMap
     retv <- liftM fst (runTclM t st)
     return (retv == v)

errWithEnv :: TclM a -> IO (Either Err a)
errWithEnv t =
    do st <- makeState' Map.empty [] emptyProcMap
       retv <- liftM fst (runTclM t st)
       return retv

commonTests = TestList [ setTests, getTests, unsetTests, withScopeTests ] where

  b = pack

  evalWithEnv :: TclM a -> IO (Either Err a, TclStack)
  evalWithEnv t =
    do st <- makeState' Map.empty [] emptyProcMap
       (retv, resStack) <- runTclM t st
       return (retv, tclStack resStack)


  checkErr a s = errWithEnv a >>= \v -> assertEqual "err match" (Left (EDie s)) v
  checkNoErr a = errWithEnv a >>= \v -> assertBool "err match" (isRight v)

  checkExists a n = do (_,(v:_)) <- evalWithEnv a
                       vExists n v

  vExists vn env = assert (Map.member (b vn) (frVars env))

  checkEq :: TclM t -> String -> T.TclObj -> Assertion
  checkEq a n val = do (_,(v:_)) <- evalWithEnv a
                       vEq n v val

  vEq vn env val = assert ((Map.lookup (b vn) (frVars env)) == (Just (ScalarVar val)))

  value = int 666
  name = b "varname"

  isRight (Right _) = True
  isRight _         = False
  int = T.mkTclInt

  setTests = TestList [
       "set exists" ~: (varSet (b "x") (int 1)) `checkExists` "x"
       ,"set exists2" ~: (varSet (b "boogie") (int 1)) `checkExists` "boogie"
       ,"checkeq" ~: checkEq (varSet name value) "varname" value
     ]
  withScopeTests = TestList [
      "with scope" ~: getVM (varSet (b "x") (int 1)) (\m -> not (Map.null m))
    ]
   where getVM f c = do (res,_) <- evalWithEnv (withScope' emptyVarMap f)
                        case res of
                         Left e -> error (show e)
                         Right (_,vm) -> assertBool "getVM" (c (frVars vm))
                        

  getTests = TestList [
       "non-exist" ~: (varGet (b "boo")) `checkErr` "can't read \"boo\": no such variable"
       ,"no err if exists" ~: checkNoErr ((varSet name value) >> varGet name)
     ]

  unsetTests = TestList [
       "non-exist" ~: (varUnset (b "boo")) `checkErr` "can't unset \"boo\": no such variable"
     ]

-- # ENDTESTS # --
