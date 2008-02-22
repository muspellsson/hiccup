{-# LANGUAGE BangPatterns #-}
module Common (RetVal, TclM
       ,TclState
       ,Err(..)
       ,TclProc,procFn,procBody
       ,runTclM
       ,makeState
       ,runCheckResult
       ,withLocalScope
       ,withNS
       ,makeProcMap
       ,mergeProcMaps
       ,getProc
       ,getProcNS
       ,regProc
       ,varGetNS
       ,varGet
       ,varModify
       ,varSet
       ,varSetHere
       ,varExists
       ,varUnset
       ,renameProc
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
       ,variableNS
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
import Data.Unique
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
         nsFrame :: FrameRef,
         nsParent :: Maybe (IORef Namespace),
         nsChildren :: Map.Map BString (IORef Namespace) } 

globalNS = do 
  vm <- frameWithVars Map.empty
  return $ TclNS { nsName = nsSep, nsFullName = pack "", nsProcs = emptyProcMap, nsFrame = vm, nsParent = Nothing, nsChildren = Map.empty }

data TclProcT = TclProcT { procName :: BString, procBody :: BString,  procFn :: TclProc }

type FrameRef = IORef TclFrame
data TclFrame = TclFrame { frVars :: VarMap, upMap :: Map.Map BString (FrameRef,BString), frTag :: Int  }
type TclStack = [FrameRef]
data TclState = TclState { tclChans :: ChanMap, tclStack :: TclStack, tclCurrNS :: IORef Namespace, tclGlobalNS :: IORef Namespace }
type TclProc = [T.TclObj] -> TclM T.TclObj
type ProcMap = Map.Map ProcKey TclProcT
type ProcKey = BString
type VarMap = Map.Map BString TclVar
data TclVar = ScalarVar T.TclObj | ArrayVar TclArray | Undefined deriving (Eq,Show)

type TclArray = Map.Map BString T.TclObj

{-
showStack = do st <- getStack
               mapM_ showFrame st

showFrame frref = do 
  fr <- readRef frref
  vars <- getFrameVars frref >>= return . Map.keys 
  let tag = frTag fr 
  updata <- mapM (\(k,(ur,un)) -> getTag ur >>= \rt -> return (k,(rt,un))) (Map.toList (upMap fr))
  (io . print) (tag,vars,updata)
-}
  

makeProcMap :: [(String,TclProc)] -> ProcMap
makeProcMap = Map.fromList . map toTclProcT . mapFst pack

mergeProcMaps :: [ProcMap] -> ProcMap
mergeProcMaps = Map.unions

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

getStack = gets tclStack
getProcMap = getCurrNS >>= (`refExtract` nsProcs)
{-# INLINE getProcMap #-}

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
  return $ pmLookup k (nsProcs ns)

getProcNorm :: ProcKey -> TclM (Maybe TclProcT)
getProcNorm i = do
  currpm <- getProcMap
  case pmLookup i currpm of
    Nothing -> do globpm <- getGlobalProcMap
                  return (pmLookup i globpm)
    x       -> return x
 where getGlobalProcMap = getGlobalNS >>= (`refExtract` nsProcs)


pmLookup :: ProcKey -> ProcMap -> Maybe TclProcT
pmLookup i m = Map.lookup i m

rmProc name = case parseNS name of
  Left _        -> rmProc' name
  Right (nsl,n) -> rmProcNS nsl n

rmProc' name = getCurrNS >>= rmFromNS name

rmFromNS pname nsref = changeProcs nsref (Map.delete pname) 

rmProcNS nsl n   
  | isGlobal (NS nsl) = rmProc' n
  | otherwise         = getNamespace nsl >>= rmFromNS n

regProc name body pr = case parseNS name of
    Left _        -> regProc' name body pr
    Right (nsl,n) -> regProcNS (NSRef (NS nsl) n) body pr

regProc' name body pr = do
   gns <- getCurrNS
   changeProcs gns (pmInsert (TclProcT name body pr))

pmInsert proc m = Map.insert (procName proc) proc m

regProcNS (NSRef Local k) body pr = regProc' k body pr
regProcNS (NSRef nst@(NS nsl) k) body pr 
 | isGlobal nst = regProc' k body pr
 | otherwise    = do  
    nsref <- getNamespace nsl
    changeProcs nsref (pmInsert (TclProcT k body pr))

varSet :: BString -> T.TclObj -> TclM RetVal
varSet !n v = varSetNS (parseVarName n) v

varSetNS (NSRef ns vn) v = runInNS ns getFrame >>= varSet' vn v

varSetHere vn v = getFrame >>= varSet' vn v

varSet' vn v frref = do
     isUpped <- upped (vnName vn) frref 
     case isUpped of
         Just (f,s) -> varSet' (vn {vnName = s}) v f
         Nothing    -> modEnv >> return v
 where modEnv = do
                ev <- getFrameVars frref
                let str = vnName vn
                case vnInd vn of
                  Nothing -> case Map.lookup str ev of
                              Just (ArrayVar _) -> tclErr $ "can't set " ++ showVN vn ++ ": variable is array"
                              _                 -> changeVars frref (Map.insert str (ScalarVar v))
                  Just i  -> case Map.findWithDefault (ArrayVar Map.empty) str ev of
                               ArrayVar prev ->  changeVars frref (Map.insert str (ArrayVar (Map.insert i v prev)))
                               Undefined     ->  changeVars frref (Map.insert str (ArrayVar (Map.singleton i v)))
                               _     -> tclErr $ "Can't set " ++ showVN vn ++ ": variable isn't array"


varModify :: BString -> (T.TclObj -> TclM T.TclObj) -> TclM RetVal
varModify !n f = do 
  let vn = parseVarName n
  val <- varGetNS vn
  res <- f val
  varSetNS vn res
                 
{-# INLINE varModify #-}

varExists :: BString -> TclM Bool
varExists name = (varGet name >> return True) `catchError` (\_ -> return False)

renameProc old new = do
  mpr <- getProc old
  case mpr of
   Nothing -> tclErr $ "bad command " ++ show old
   Just pr -> do rmProc old
                 unless (B.null new) (regProc new (procBody pr) (procFn pr))

varUnset :: BString -> TclM RetVal
varUnset name = do
  let (NSRef ns (VarName n _)) = parseVarName name
  runInNS ns (varUnset2 n)
  ret

runInNS !ns f
  | isLocal ns  = f
  | isGlobal ns = upglobal f
  | otherwise   = do let (NS nsl) = ns
                     nsref <- getNamespace nsl
                     withExistingNS nsref f
{-# INLINE runInNS #-}

varUnset2 name = do
  frref <- getFrame
  varUnset' name frref

varUnset' name frref = do
   isUpped <- upped name frref
   case isUpped of
      Nothing    -> do vmap <- getFrameVars frref
                       verifyNameIn vmap
                       changeVars frref (Map.delete name)
      Just (f,s) -> do umap <- getUpMap frref
                       verifyNameIn umap
                       changeUpMap frref (Map.delete name)
                       varUnset' s f
 where bad = tclErr ("can't unset " ++ show name ++ ": no such variable")
       verifyNameIn m = unless (Map.member name m) bad

getArray :: BString -> TclM TclArray
getArray name = do
  let (NSRef ns (VarName nm _)) = parseVarName name
  runInNS ns (getFrame >>= getArray' nm)

getArray' name frref = do
   var <- varLookup name frref
   case var of
      Just (ArrayVar a) -> return a
      Just _            -> tclErr $ "can't read " ++ show name ++ ": variable isn't array"
      Nothing           -> tclErr $ "can't read " ++ show name ++ ": no such variable"

varLookup name frref = do
   isUpped <- upped name frref
   case isUpped of
      Nothing    -> getFrameVars frref >>= return . Map.lookup name
      Just (f,n) -> varLookup n f

varGet :: BString -> TclM RetVal
varGet !n = varGetNS (parseVarName n)

varGetNS :: NSRef VarName -> TclM RetVal
varGetNS (NSRef ns vn) = runInNS ns (varGet2 vn)
{-# INLINE varGetNS #-}

varGet2 vn = do
  frref <- getFrame
  varGet' vn frref

varGet' vn@(VarName name ind) frref = do
  var <- varLookup name frref
  case var of
   Nothing -> cantReadErr "no such variable"
   Just o  -> o `withInd` ind
 where cantReadErr why  = tclErr $ "can't read " ++ showVN vn ++ ": " ++ why
       withInd (ScalarVar o) Nothing = return o
       withInd (ScalarVar _) _       = cantReadErr "variable isn't array"
       withInd (ArrayVar o) (Just i) = maybe (cantReadErr "no such element in array") return (Map.lookup i o)
       withInd (ArrayVar _)  _       = cantReadErr "variable is array"
       withInd Undefined     _       = cantReadErr "no such variable"


uplevel :: Int -> TclM a -> TclM a
uplevel i p = do
  (curr,new) <- liftM (splitAt i) getStack
  when (null new) (tclErr ("bad level: " ++ show i))
  putStack new
  res <- p `ensure` (modStack (curr ++))
  return res
{-# INLINE uplevel #-}

upglobal :: TclM a -> TclM a
upglobal p = do
  len <- stackLevel
  uplevel len p

getUpFrame i = do st <- getStack
                  if length st <= i
                      then fail "too far up the stack"
                      else return (st!!i)
                  
linkToFrame name (upfr, upname) = do
  frref <- getFrame
  changeUpMap frref (Map.insert name (upfr, upname))

upvar n d s = do
   upfr <- getUpFrame n
   s `linkToFrame` (upfr, d)
{-# INLINE upvar #-}

deleteNS name = do 
 let nsl = explodeNS name
 ns <- getNamespace nsl >>= readRef
 case nsParent ns of
   Nothing -> return ()
   Just p -> removeChild p (last nsl)

removeChild nsr child = io (modifyIORef nsr (\v -> v { nsChildren = Map.delete child (nsChildren v) } ))

getNamespace nsl = case nsl of
       (x:xs) -> do base <- if B.null x then getGlobalNS else getCurrNS >>= getKid x
                    getEm xs base
       []     -> fail "Something unexpected happened in getNamespace"
 where getEm []     ns = return ns
       getEm (x:xs) ns = getKid x ns >>= getEm xs
       getKid k nsref = do kids <- nsref `refExtract`  nsChildren
                           case Map.lookup k kids of
                               Nothing -> fail $ "can't find namespace " ++ show k
                               Just v  -> return v

existsNS ns = (getNamespace (explodeNS ns) >> return True) `catchError` (\_ -> return False)


variableNS name val = do
  let (NSRef ns (VarName n _)) = parseVarName name
  nsref <- destNS ns
  nsfr <- getNSFrame nsref
  fr <- getFrame
  same <- sameTags fr nsfr
  if same then changeVars fr (Map.insert name varVal)
          else n `linkToFrame` (nsfr, n)
  ret
 where
   destNS Local    = getCurrNS
   destNS (NS nsl) = getNamespace nsl
   varVal = maybe Undefined ScalarVar val
   sameTags f1 f2 = do
      t1 <- getTag f1
      t2 <- getTag f2
      return (t1 == t2)
  
  
getTag frref = do
  f <- readRef frref
  return (frTag f)

withLocalScope vl f = do
    vm <- io $ frameWithVars $ (Map.fromList . mapSnd ScalarVar) vl
    withScope' vm f

withScope' :: FrameRef -> TclM a -> TclM a
withScope' frref fun = do
  (o:old) <- getStack
  putStack $ frref : o : old
  fun `ensure` (modStack (drop 1))

mkEmptyNS name parent = do
    pname <- liftM nsFullName (readIORef parent)
    emptyVM <- frameWithVars emptyVarMap
    let fullname = B.concat [pname, nsSep, name]
    new <- newIORef $ TclNS { nsName = name, nsFullName = fullname, nsProcs = emptyProcMap, nsFrame = emptyVM, nsParent = Just parent, nsChildren = Map.empty }
    modifyIORef parent (\n -> n { nsChildren = Map.insert name new (nsChildren n) })
    return new

withNS :: BString -> TclM a -> TclM a
withNS name f = do
     newCurr <- getOrCreateNamespace name
     withExistingNS newCurr f


withExistingNS newCurr f = do
     nsref <- getCurrNS
     putCurrNS newCurr
     (op newCurr) `ensure` (putCurrNS nsref)
 where op nsr = do vm <- getNSFrame nsr
                   withScope' vm f

getFrameVars :: FrameRef -> TclM VarMap
getFrameVars = (`refExtract` frVars)

getUpMap = (`refExtract` upMap)

getNSFrame :: IORef Namespace -> TclM FrameRef
getNSFrame nsref = nsref `refExtract` nsFrame 

getOrCreateNamespace ns = case explodeNS ns of
       (x:xs) -> do base <- if B.null x then getGlobalNS else getCurrNS >>= getKid x
                    getEm xs base
       []     -> fail "Something unexpected happened in getOrCreateNamespace"
 where getEm []     ns = return ns
       getEm (x:xs) ns = getKid x ns >>= getEm xs
       getKid k nsref = do kids <- nsref `refExtract` nsChildren
                           case Map.lookup k kids of
                               Nothing -> io (mkEmptyNS k nsref)
                               Just v  -> return v

putCurrNS ns = modify (\s -> s { tclCurrNS = ns })
getCurrNS = gets tclCurrNS

getGlobalNS = gets tclGlobalNS

readRef = io . readIORef
{-# INLINE readRef #-}

refExtract ref f = readRef ref >>= return . f
{-# INLINE refExtract #-}

currentNS = do
  ns <- getCurrNS >>= readRef
  return (nsFullName ns)

parentNS = do
 ns <- getCurrNS >>= readRef
 case nsParent ns of
   Nothing -> return B.empty
   Just v  -> readRef v >>= return . nsFullName

childrenNS :: TclM [BString]
childrenNS = do
  ns <- getCurrNS >>= readRef
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

frameWithVars vref = do
   tag <- liftM hashUnique newUnique
   newIORef $ TclFrame { frVars = vref, upMap = Map.empty, frTag = tag }

changeUpMap fr fun = io (modifyIORef fr (\f -> f { upMap = fun (upMap f) }))

changeVars !fr fun = io (modifyIORef fr (\f -> f { frVars = fun (frVars f) } ))
{-# INLINE changeVars #-}

changeProcs nsr fun = io (modifyIORef nsr (\f -> f { nsProcs = fun (nsProcs f) } ))

makeEnsemble name subs = top
  where top args = case args of
                   (x:xs) -> case Map.lookup (T.asBStr x) subMap of
                              Nothing -> tclErr $ "unknown subcommand " ++ show (T.asBStr x) ++ ": must be " ++ commaList "or" (map unpack (procMapNames subMap))
                              Just f  -> (procFn f) xs
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
