{-# LANGUAGE BangPatterns #-}
module Common (RetVal, TclM
       ,TclState
       ,Err(..)
       ,TclProc,TclProcT(..)
       ,runTclM
       ,makeState
       ,runCheckResult
       ,withLocalScope
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
       ,deleteNS
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


type RetVal = T.TclObj 

data Err = ERet !RetVal | EBreak | EContinue | EDie String deriving (Eq,Show)

instance Error Err where
 noMsg    = EDie "An error occurred."
 strMsg s = EDie s

type TclM = ErrorT Err (StateT TclState IO)

data Namespace = TclNS {
         nsName :: BString,
         nsFullName :: BString,
         nsProcs :: ProcMap,
         nsVars :: IORef TclFrame,
         nsParent :: Maybe (IORef Namespace),
         nsChildren :: Map.Map BString (IORef Namespace) } 

globalNS = do 
  vm <- frameWithVars Map.empty
  return $ TclNS { nsName = nsSep, nsFullName = pack "", nsProcs = emptyProcMap, nsVars = vm, nsParent = Nothing, nsChildren = Map.empty }

data TclProcT = TclProcT { procName :: BString, procBody :: BString,  procFunction :: TclProc }

data TclFrame = TclFrame { frVars :: VarMap, upMap :: Map.Map BString (Int,BString) }
type TclStack = [IORef TclFrame]
data TclState = TclState { tclChans :: ChanMap, tclStack :: TclStack, tclCurrNS :: IORef Namespace, tclGlobalNS :: IORef Namespace }
type TclProc = [T.TclObj] -> TclM T.TclObj
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
makeState' chans vlist procs = do fr <- frameWithVars (Map.fromList (mapSnd ScalarVar vlist))
                                  gns <- globalNS
                                  ns <- newIORef (gns { nsProcs = procs })
                                  return (TclState chans [fr] ns ns)

stackLevel = getStack >>= return . pred . length
globalVars = upglobal localVars
localVars = getFrame >>= getFrameVars >>= return . Map.keys 
currentVars = do f <- getFrame
                 vs <- getFrameVars f
                 mv <- getUpMap f
                 return $ Map.keys vs ++ Map.keys mv

commandNames = getProcMap >>= return . map procName . Map.elems

getStack = do s <- gets tclStack -- TODO: Guard for empty stack here?
              case s of
                []    -> tclErr "Aack. Tried to go up too far in the stack."
                v     -> return v
getProcMap = do currNs <- gets tclCurrNS >>= readRef
                return (nsProcs currNs)
{-# INLINE getProcMap #-}
getGlobalProcMap = do gNs <- gets tclGlobalNS >>= readRef
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

tclErr = throwError . EDie

argErr s = fail ("wrong # of args: " ++ s)

runTclM :: TclM a -> TclState -> IO (Either Err a, TclState)
runTclM code env = runStateT (runErrorT code) env

onChan f = gets tclChans >>= f
modChan f = modify (\s -> s { tclChans = f (tclChans s) })
getChan n = onChan (\m -> return (lookupChan n m))
addChan c    = modChan (insertChan c)
removeChan c = modChan (deleteChan c)

upped s fr = readRef fr >>= \f ->  return (Map.lookup s (upMap f))
{-# INLINE upped #-}

getProc pname = case parseNS pname of
    Left n -> getProcNorm n
    Right (nsl,n) -> getProcNS (NSRef (NS nsl) n)

getProcNS (NSRef Local k) = getProcNorm k
getProcNS (NSRef (NS nsl) k) = do
  nsref <- getNamespace nsl
  ns <- readRef nsref
  return $ getProc' k (nsProcs ns)

deleteNS name = do 
 let nsl = explodeNS name
 ns <- getNamespace nsl >>= readRef
 case nsParent ns of
   Nothing -> return ()
   Just p -> removeChild p (last nsl)

removeChild nsr child = io (modifyIORef nsr (\v -> v { nsChildren = Map.delete child (nsChildren v) } ))
                   

getNamespace nsl = case nsl of
       (x:xs) -> do base <- if B.null x then gets tclGlobalNS else gets tclCurrNS >>= getKid x
                    getEm xs base
       []     -> fail "Something unexpected happened in getNamespace"
 where getEm []     ns = return ns
       getEm (x:xs) ns = getKid x ns >>= getEm xs
       getKid k nsref = do kids <- readRef nsref >>= return . nsChildren
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

varSet' :: VarName -> T.TclObj -> TclM RetVal
varSet' vn v = do
     frref <- getFrame
     isUpped <- upped (vnName vn) frref 
     case isUpped of
         Just (i,s) -> uplevel i (varSet' (vn {vnName = s}) v)
         Nothing    -> modEnv frref >> return v
 where modEnv frref = do
                ev <- getFrameVars frref
                let str = vnName vn
                case vnInd vn of
                  Nothing -> case Map.lookup str ev of
                              Just (ArrayVar _) -> tclErr $ "can't set " ++ showVN vn ++ ": variable is array"
                              _                 -> changeVars frref (Map.insert str (ScalarVar v))
                  Just i  -> case Map.findWithDefault (ArrayVar Map.empty) str ev of
                               ScalarVar _     -> tclErr $ "Can't set " ++ showVN vn ++ ": variable isn't array"
                               ArrayVar prev ->  changeVars frref (Map.insert str (ArrayVar (Map.insert i v prev)))


varModify :: BString -> (T.TclObj -> TclM T.TclObj) -> TclM RetVal
varModify !n f = do 
  let vn = parseVarName n
  val <- varGetNS vn
  res <- f val
  varSetNS vn res
                 
{-# INLINE varModify #-}

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
  ret

runInNS !ns f
  | isLocal ns  = f
  | isGlobal ns = upglobal f
  | otherwise   = do let (NS nsl) = ns
                     nsref <- getNamespace nsl
                     withExistingNS nsref f
{-# INLINE runInNS #-}

varUnset' :: BString -> TclM ()
varUnset' name = do
   frref <- getFrame
   isUpped <- upped name frref
   case isUpped of
      Nothing    -> do vmap <- getFrameVars frref
                       verifyNameIn vmap
                       changeVars frref (Map.delete name)
      Just (i,s) -> do umap <- getUpMap frref
                       verifyNameIn umap
                       changeUpMap frref (Map.delete name)
                       uplevel i (varUnset' s) 
 where bad = tclErr ("can't unset " ++ show name ++ ": no such variable")
       verifyNameIn m = unless (Map.member name m) bad

getArray :: BString -> TclM TclArray
getArray name = do
   var <- varLookup name
   case var of
      Just (ArrayVar a) -> return a
      Just _            -> tclErr $ "can't read " ++ show name ++ ": variable isn't array"
      Nothing           -> tclErr $ "can't read " ++ show name ++ ": no such variable"

varLookup :: BString -> TclM (Maybe TclVar)
varLookup name = do
   frref <- getFrame
   isUpped <- upped name frref
   case isUpped of
      Nothing    -> getFrameVars frref >>= return . Map.lookup name
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

upvar n d s = do frref <- getFrame
                 changeUpMap frref (Map.insert (T.asBStr s) (n, T.asBStr d))
{-# INLINE upvar #-}

withLocalScope vl f = do
    vm <- io $ frameWithVars $ (Map.fromList . mapSnd ScalarVar) vl
    withScope' vm f

withScope' :: IORef TclFrame -> TclM a -> TclM a
withScope' frref fun = do
  (o:old) <- getStack
  putStack $ frref : o : old
  fun `ensure` (modStack (drop 1))

mkEmptyNS name parent = do
    pname <- liftM nsFullName (readIORef parent)
    emptyVM <- frameWithVars emptyVarMap
    let fullname = B.concat [pname, nsSep, name]
    new <- newIORef $ TclNS { nsName = name, nsFullName = fullname, nsProcs = emptyProcMap, nsVars = emptyVM, nsParent = Just parent, nsChildren = Map.empty }
    modifyIORef parent (\n -> n { nsChildren = Map.insert name new (nsChildren n) })
    return new

withNS :: BString -> TclM a -> TclM a
withNS name f = do
     newCurr <- getOrCreateNamespace name
     withExistingNS newCurr f

withExistingNS newCurr f = do
     nsref <- gets tclCurrNS
     putCurrNS newCurr
     (op newCurr) `ensure` (putCurrNS nsref)
 where op nsr = do vm <- getNSFrame nsr
                   withScope' vm f

getFrameVars :: IORef TclFrame -> TclM VarMap
getFrameVars frref = readRef frref >>= return . frVars

getUpMap frref = readRef frref >>= return . upMap

getNSFrame :: IORef Namespace -> TclM (IORef TclFrame)
getNSFrame nsref = readRef nsref >>= return . nsVars 

getOrCreateNamespace ns = case explodeNS ns of
       (x:xs) -> do base <- if B.null x then gets tclGlobalNS else gets tclCurrNS >>= getKid x
                    getEm xs base
       []     -> fail "Something unexpected happened in getOrCreateNamespace"
 where getEm []     ns = return ns
       getEm (x:xs) ns = getKid x ns >>= getEm xs
       getKid k nsref = do kids <- readRef nsref >>= return . nsChildren
                           case Map.lookup k kids of
                               Nothing -> io (mkEmptyNS k nsref)
                               Just v  -> return v

putCurrNS ns = modify (\s -> s { tclCurrNS = ns })

readRef = io . readIORef

currentNS = do
  ns <- gets tclCurrNS >>= readRef
  return (nsFullName ns)

parentNS = do
 ns <- gets tclCurrNS >>= readRef
 case nsParent ns of
   Nothing -> return B.empty
   Just v  -> readRef v >>= return . nsFullName

childrenNS :: TclM [BString]
childrenNS = do
  ns <- gets tclCurrNS >>= readRef
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

frameWithVars vref = newIORef $ TclFrame { frVars = vref, upMap = Map.empty }

changeUpMap fr fun = io (modifyIORef fr (\f -> f { upMap = fun (upMap f) }))

changeVars fr fun = io (modifyIORef fr (\f -> f { frVars = fun (frVars f) } ))
{-# INLINE changeVars #-}

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

runCheckResult :: TclM RetVal -> Either Err RetVal -> IO Bool
runCheckResult t v =
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

  vExists vn env = readIORef env >>= \fr -> return (frVars fr) >>= \vm -> assert (Map.member (b vn) vm)

  checkEq :: TclM t -> String -> T.TclObj -> Assertion
  checkEq a n val = do (_,(v:_)) <- evalWithEnv a
                       vEq n v val

  vEq vn env val = do
     vm <- readIORef env >>= return . frVars 
     assert ((Map.lookup (b vn) vm) == (Just (ScalarVar val)))

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
   where getVM f c = do vmr <- frameWithVars emptyVarMap 
                        (res,_) <- evalWithEnv (withScope' vmr f)
                        case res of
                         Left e -> error (show e)
                         Right _ -> do fr <- readIORef vmr
                                       vm <- return (frVars fr) 
                                       assertBool "getVM" (c vm)
                        

  getTests = TestList [
       "non-exist" ~: (varGet (b "boo")) `checkErr` "can't read \"boo\": no such variable"
       ,"no err if exists" ~: checkNoErr ((varSet name value) >> varGet name)
     ]

  unsetTests = TestList [
       "non-exist" ~: (varUnset (b "boo")) `checkErr` "can't unset \"boo\": no such variable"
     ]

-- # ENDTESTS # --
