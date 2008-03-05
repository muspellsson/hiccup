{-# LANGUAGE BangPatterns #-}
module Common (RetVal, TclM
       ,TclState
       ,Err(..)
       ,TclCmd,applyTo,procBody
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
       ,varUnsetNS
       ,renameProc
       ,getArray
       ,addChan
       ,removeChan
       ,getChan
       ,evtAdd
       ,evtGetDue
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
       ,exportNS
       ,importNS
       ,commandNames
       ,commonTests
    ) where

import qualified Data.ByteString.Char8 as B
import Control.Monad.Error
import Control.Monad.State.Strict
import qualified Data.Map as Map
import Data.IORef
import Data.Unique

import qualified EventMgr as Evt

import qualified TclObj as T
import TclChan
import VarName
import Util

import Test.HUnit


type RetVal = T.TclObj 

data Err = ERet !RetVal | EBreak 
          | EContinue | EDie String deriving (Eq,Show)

instance Error Err where
 noMsg    = EDie "An error occurred."
 strMsg s = EDie s

type TclM = ErrorT Err (StateT TclState IO)

data Namespace = TclNS {
         nsName :: BString,
         nsProcs :: ProcMap,
         nsFrame :: FrameRef,
         nsExport :: [BString],
         nsParent :: Maybe NSRef,
         nsChildren :: Map.Map BString NSRef } 


type FrameRef = IORef TclFrame
type NSRef = IORef Namespace

data TclFrame = TclFrame { 
      frVars :: !VarMap, 
      upMap :: Map.Map BString (FrameRef,BString), 
      frNS :: NSRef,
      frTag :: Int  }

type TclStack = [FrameRef]

data TclState = TclState { 
    tclChans :: ChanMap, 
    tclEvents :: Evt.EventMgr T.TclObj,
    tclStack :: TclStack, 
    tclGlobalNS :: !NSRef }

type TclCmd = [T.TclObj] -> TclM T.TclObj
data TclProcT = TclProcT { 
                   procName :: BString, 
                   procBody :: BString,  
                   procOrigin :: NSTag,
                   procFn :: TclCmd }
type ProcKey = BString
type ProcMap = Map.Map ProcKey TclProcT

type TclArray = Map.Map BString T.TclObj
data TclVar = ScalarVar !T.TclObj | ArrayVar TclArray | Undefined deriving (Eq,Show)
type VarMap = Map.Map BString TclVar

applyTo !(TclProcT _ _ _ !f) !args = f args
{-# INLINE applyTo #-}

mkProcAlias nsr pn args = do
  pm <- nsr `refExtract` nsProcs
  case pmLookup pn pm of
    Nothing -> tclErr "bad imported command. Yikes"
    Just p  -> p `applyTo` args
  
   
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
  
globalNS fr = do 
  return $ TclNS { nsName = nsSep, 
                   nsProcs = emptyProcMap, nsFrame = fr, 
                   nsExport = [],
                   nsParent = Nothing, nsChildren = Map.empty }

makeProcMap :: [(String,TclCmd)] -> ProcMap
makeProcMap = Map.fromList . map toTclProcT . mapFst pack

toTclProcT (n,v) = (n, TclProcT n errStr Local v)
 where errStr = pack $ show n ++ " isn't a procedure"

mergeProcMaps :: [ProcMap] -> ProcMap
mergeProcMaps = Map.unions

makeState :: [(BString,T.TclObj)] -> ProcMap -> IO TclState
makeState = makeState' baseChans

makeState' :: ChanMap -> [(BString,T.TclObj)] -> ProcMap -> IO TclState
makeState' chans vlist procs = do fr <- createFrame (Map.fromList (mapSnd ScalarVar vlist))
                                  gns <- globalNS fr
                                  ns <- newIORef (gns { nsProcs = procs })
                                  setFrNS fr ns
                                  return $! TclState {  tclChans = chans,
				                                                tclEvents = Evt.emptyMgr,
                                                        tclStack = [fr], 
                                                        tclGlobalNS = ns } 

getStack = gets tclStack
{-# INLINE getStack  #-}
getNsProcMap = getCurrNS >>= (`refExtract` nsProcs)
{-# INLINE getNsProcMap #-}
getGlobalProcMap = getGlobalNS >>= (`refExtract` nsProcs)

putStack s = modify (\v -> v { tclStack = s })
modStack :: (TclStack -> TclStack) -> TclM ()
modStack f = modify (\v -> v { tclStack = f (tclStack v) })
{-# INLINE modStack #-}
getFrame = do st <- getStack
              case st of
                 (fr:_) -> return $! fr
                 _      -> tclErr "stack badness"

{-# INLINE putStack  #-}
{-# INLINE getFrame  #-}

io :: IO a -> TclM a
io = liftIO
{-# INLINE io #-}

stackLevel = getStack >>= return . pred . length
globalVars = getGlobalNS >>= getNSFrame >>= getFrameVars >>= return . Map.keys 
localVars = getFrame >>= getFrameVars >>= return . Map.keys 
currentVars = do f <- getFrame
                 vs <- getFrameVars f
                 mv <- getUpMap f
                 return $ Map.keys vs ++ Map.keys mv

commandNames = getNsProcMap >>= return . map procName . Map.elems


tclErr = throwError . EDie

argErr s = fail ("wrong # of args: " ++ s)

runTclM :: TclM a -> TclState -> IO (Either Err a, TclState)
runTclM code env = runStateT (runErrorT code) env

onChan f = gets tclChans >>= f
modChan f = modify (\s -> s { tclChans = f (tclChans s) })
getChan n = onChan (\m -> return (lookupChan n m))
addChan c    = modChan (insertChan c)
removeChan c = modChan (deleteChan c)

evtAdd e t = do 
  em <- gets tclEvents
  (tag,m) <- io $ Evt.addEvent e t em
  modify (\s -> s { tclEvents = m })
  treturn tag

evtGetDue = do
  em <- gets tclEvents
  (d,em') <- io $ Evt.getDue em
  when (not (null d)) $ modify (\s -> s { tclEvents = em' })
  return d

upped !s !fr = getUpMap fr >>= \f -> return $! (Map.lookup s f)
{-# INLINE upped #-}

getProc !pname = getProcNS (parseProc pname)

getProcNS (NSQual Local n) = getProcNorm n
getProcNS (NSQual nst n) = do
  nsref <- getNamespace nst
  procs <- nsref `refExtract` nsProcs
  return $! pmLookup n procs
{-# INLINE getProcNS #-}

getProcNorm :: ProcKey -> TclM (Maybe TclProcT)
getProcNorm !i = do
  currpm <- getNsProcMap
  case pmLookup i currpm of
    Nothing -> do globpm <- getGlobalProcMap
                  return (pmLookup i globpm)
    x       -> return $! x

pmLookup :: ProcKey -> ProcMap -> Maybe TclProcT
pmLookup !i !m = Map.lookup i m
{-# INLINE pmLookup #-}

rmProc name = rmProcNS (parseProc name)
rmProcNS (NSQual nst n)
  | isLocal  nst = getCurrNS >>= rmFromNS n
  | isGlobal nst = getGlobalNS >>= rmFromNS n
  | otherwise    = getNamespace nst >>= rmFromNS n
 where rmFromNS pname nsref = changeProcs nsref (Map.delete pname) 

regProc name body pr = regProcNS (parseProc name) body pr

regProcNS (NSQual nst k) body pr 
 | isGlobal nst = getGlobalNS >>= regInNS
 | isLocal nst  = getCurrNS >>= regInNS
 | otherwise    = getNamespace nst >>= regInNS
 where 
  newProc = TclProcT k body nst pr
  pmInsert proc m = Map.insert (procName proc) proc m
  regInNS nsr = changeProcs nsr (pmInsert newProc)

varSet :: BString -> T.TclObj -> TclM RetVal
varSet !n v = varSetNS (parseVarName n) v

varSetNS qvn v = usingNsFrame qvn (\n f -> varSet' n v f)
{-# INLINE varSetNS #-}

varSetHere vn v = getFrame >>= varSet' vn v

varSet' vn v frref = do
     isUpped <- upped (vnName vn) frref 
     case isUpped of
         Nothing    -> modVar (vnName vn) >> return v
         Just (f,s) -> varSet' (vn {vnName = s}) v f
 where cantSetErr why = fail $ "can't set " ++ showVN vn ++ ":" ++ why
       modVar str = do
         vm <- getFrameVars frref
         let changeVar = insertVar frref str
         case vnInd vn of
           Nothing -> case Map.lookup str vm of
                        Just (ArrayVar _) -> cantSetErr "variable is array"
                        _                 -> changeVar (ScalarVar v)
           Just i  -> case Map.findWithDefault (ArrayVar Map.empty) str vm of
                        ArrayVar prev ->  changeVar (ArrayVar (Map.insert i v prev))
                        Undefined     ->  changeVar (ArrayVar (Map.singleton i v))
                        _     -> cantSetErr "variable isn't array"


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
                 unless (bsNull new) (regProc new (procBody pr) (procFn pr))

varUnset :: BString -> TclM RetVal
varUnset name = varUnsetNS (parseVarName name)

varUnsetNS qns = usingNsFrame qns varUnset' >> ret

usingNsFrame (NSQual !ns !vn) f = lookupNsFrame ns >>= f vn
 where lookupNsFrame Local = getFrame 
       lookupNsFrame !ns   = getNamespace ns >>= getNSFrame
{-# INLINE usingNsFrame #-}

varUnset' vn frref = do
     isUpped <- upped (vnName vn) frref 
     case isUpped of
         Nothing    -> modVar 
         Just (f,s) -> do 
             when (not (isArr vn)) $ do 
                 changeUpMap frref (Map.delete (vnName vn))
             varUnset' (vn {vnName = s}) f
 where noExist = cantUnset "no such variable" 
       cantUnset why = fail $ "can't unset " ++ showVN vn ++ ": " ++ why
       modVar = do
         vm <- getFrameVars frref
         let str = vnName vn
         let deleteVar = changeVars frref (Map.delete str)
         val <- maybe noExist return (Map.lookup str vm)
         case vnInd vn of
           Nothing -> deleteVar
           Just i  -> case val of
                        ArrayVar prev -> case Map.lookup i prev of 
                                           Nothing -> cantUnset "no such element in array"
                                           Just _  -> insertVar frref str (prev `modArr` (Map.delete i))
                        ScalarVar _   -> cantUnset "variable isn't array"
                        _             -> noExist

modArr v f = ArrayVar (f v)

getArray :: BString -> TclM TclArray
getArray name = usingNsFrame (parseProc name) getArray'

getArray' name frref = do
   var <- varLookup name frref
   case var of
      Just (ArrayVar a) -> return a
      Just _            -> tclErr $ "can't read " ++ show name ++ ": variable isn't array"
      Nothing           -> tclErr $ "can't read " ++ show name ++ ": no such variable"

varLookup !name !frref = do
   isUpped <- upped name frref
   case isUpped of
      Nothing    -> getFrameVars frref >>= \m -> return $! (Map.lookup name m)
      Just (f,n) -> varLookup n f

varGet :: BString -> TclM RetVal
varGet !n = varGetNS (parseVarName n)

varGetNS :: NSQual VarName -> TclM RetVal
varGetNS qns = usingNsFrame qns varGet'
{-# INLINE varGetNS #-}

varGet' vn !frref = do
  var <- varLookup (vnName vn) frref
  case var of
   Nothing -> cantReadErr "no such variable"
   Just o  -> o `getInd` (vnInd vn)
 where cantReadErr why  = fail $ "can't read " ++ showVN vn ++ ": " ++ why
       getInd (ScalarVar o) Nothing = return o
       getInd (ScalarVar _) _       = cantReadErr "variable isn't array"
       getInd (ArrayVar o) (Just i) = maybe (cantReadErr "no such element in array") return (Map.lookup i o)
       getInd (ArrayVar _)  _       = cantReadErr "variable is array"
       getInd Undefined     _       = cantReadErr "no such variable"


uplevel :: Int -> TclM a -> TclM a
uplevel i p = do
  (curr,new) <- liftM (splitAt i) getStack
  when (null new) (tclErr ("bad level: " ++ show i))
  putStack new
  res <- p `ensure` (modStack (curr ++))
  return res
{-# INLINE uplevel #-}

getUpFrame i = do st <- getStack
                  if length st <= i
                      then fail "too far up the stack"
                      else return $! (st!!i)
                  
linkToFrame name (upfr, upname) = do
  frref <- getFrame
  changeUpMap frref (Map.insert name (upfr, upname))

upvar n d s = do
   upfr <- getUpFrame n
   s `linkToFrame` (upfr, d)
{-# INLINE upvar #-}

deleteNS name = do 
 let nsl = explodeNS name
 ns <- getNamespace' nsl >>= readRef
 case nsParent ns of
   Nothing -> return ()
   Just p -> removeChild p (last nsl)

removeChild nsr child = io (modifyIORef nsr (\v -> v { nsChildren = Map.delete child (nsChildren v) } ))

getNamespace Local = getCurrNS
getNamespace (NS nsl) = getNamespace' nsl
{-# INLINE getNamespace #-}
getNamespace' nsl = case nsl of
       (x:xs) -> do base <- if bsNull x then getGlobalNS else getCurrNS >>= getKid x
                    getEm xs base
       []     -> fail "Something unexpected happened in getNamespace"
 where getEm []     ns = return $! ns
       getEm (x:xs) ns = getKid x ns >>= getEm xs
       getKid k nsref = do kids <- nsref `refExtract`  nsChildren
                           case Map.lookup k kids of
                               Nothing -> fail $ "can't find namespace " ++ show k
                               Just v  -> return $! v

existsNS ns = (getNamespace' (explodeNS ns) >> return True) `catchError` (\_ -> return False)

variableNS name val = do
  let (NSQual ns (VarName n ind)) = parseVarName name
  ensureNotArr ind
  nsfr <- getNamespace ns >>= getNSFrame
  fr <- getFrame
  same <- sameTags fr nsfr
  if same then insertVar fr name varVal
          else n `linkToFrame` (nsfr, n)
 where
   ensureNotArr Nothing  = return ()
   ensureNotArr (Just _) = tclErr $ "can't define " ++ show name ++ ": name refers to value in array"
   varVal = maybe Undefined ScalarVar val
   sameTags f1 f2 = do
      t1 <- getTag f1
      t2 <- getTag f2
      return (t1 == t2)

exportNS name = do
  nsr <- getCurrNS
  io $ modifyIORef nsr (\n -> n { nsExport = (name:(nsExport n)) })

importNS name = do
    let (NSQual nst n) = parseProc name
    nsr <- getNamespace nst
    exported <- getExports nsr n
    mapM (\pn -> regProcNS (NSQual Local pn) (pack "oh noes") (mkProcAlias nsr pn)) exported
    return . T.mkTclList . map T.mkTclBStr $ exported
 where getExports nsr pat = nsr `refExtract` nsExport >>= return . globMatches pat


getTag frref = do
  f <- readRef frref
  return (frTag f)

setFrNS !frref !nsr = modifyIORef frref (\f -> f { frNS = nsr })

withLocalScope vl f = do
    fr <- io $! createFrame $! (Map.fromList . mapSnd ScalarVar) vl
    getCurrNS >>= (liftIO . setFrNS fr) 
    withScope fr f

withScope :: FrameRef -> TclM a -> TclM a
withScope !frref fun = do
  stack <- getStack
  putStack $ frref : stack
  fun `ensure` (modStack (drop 1))

mkEmptyNS name parent = do
    pname <- liftM nsName (readIORef parent)
    emptyFr <- createFrame emptyVarMap
    let sep = if pname == nsSep then B.empty else nsSep
    let fullname = B.concat [pname, sep, name]
    new <- newIORef $ TclNS { nsName = fullname, 
                              nsProcs = emptyProcMap, nsFrame = emptyFr, 
                              nsExport = [],
                              nsParent = Just parent, nsChildren = Map.empty }
    modifyIORef parent (\n -> n { nsChildren = Map.insert name new (nsChildren n) })
    setFrNS emptyFr new
    return $! new

withNS :: BString -> TclM a -> TclM a
withNS name f = do
     newCurr <- getOrCreateNamespace name
     withExistingNS f newCurr

withExistingNS f !nsref = do
  fr <- getNSFrame nsref
  withScope fr f

getFrameVars :: FrameRef -> TclM VarMap
getFrameVars !frref = (frref `refExtract` frVars) >>= \r -> return $! r
{-# INLINE getFrameVars #-}

getUpMap !frref = (frref `refExtract` upMap) >>= \r -> return $! r
{-# INLINE getUpMap #-}

getNSFrame :: NSRef -> TclM FrameRef
getNSFrame !nsref = nsref `refExtract` nsFrame 

getOrCreateNamespace ns = case explodeNS ns of
       (x:xs) -> do base <- if bsNull x then getGlobalNS else getCurrNS >>= getKid x
                    getEm xs base
       []     -> fail "Something unexpected happened in getOrCreateNamespace"
 where getEm []     ns = return ns
       getEm (x:xs) ns = getKid x ns >>= getEm xs
       getKid k nsref = do kids <- nsref `refExtract` nsChildren
                           case Map.lookup k kids of
                               Nothing -> io (mkEmptyNS k nsref)
                               Just v  -> return v

getCurrNS = getFrame >>= (`refExtract` frNS) >>= \n -> return $! n 
{-# INLINE getCurrNS #-}

getGlobalNS = gets tclGlobalNS

readRef !r = (io . readIORef) r >>= \v -> return $! v
{-# INLINE readRef #-}

refExtract !ref !f = readRef ref >>= \d -> let r = f d in r `seq` (return $! r)
{-# INLINE refExtract #-}

currentNS = do
  ns <- getCurrNS >>= readRef
  return (nsName ns)

parentNS = do
 ns <- getCurrNS >>= readRef
 case nsParent ns of
   Nothing -> return B.empty
   Just v  -> readRef v >>= return . nsName

childrenNS :: TclM [BString]
childrenNS = do
  ns <- getCurrNS >>= readRef
  (return . Map.keys . nsChildren) ns

ensure action p = do
   r <- action `catchError` (\e -> p >> throwError e)
   p
   return $! r

ret :: TclM RetVal
ret = return T.empty
{-# INLINE ret #-}

treturn :: BString -> TclM RetVal
treturn = return . T.mkTclBStr
{-# INLINE treturn #-}

createFrame !vref = do
   tag <- liftM hashUnique newUnique
   res <- newIORef $! TclFrame { frVars = vref, upMap = Map.empty, frTag = tag, frNS = undefined }
   return $! res

changeUpMap fr fun = io (modifyIORef fr (\f -> f { upMap = fun (upMap f) }))

changeVars !fr fun = io (modifyIORef fr (\f -> let r = fun (frVars f) in r `seq` f { frVars = r } ))
{-# INLINE changeVars #-}

insertVar fr k v = changeVars fr (Map.insert k v)
{-# INLINE insertVar #-}

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
   where getVM f c = do vmr <- createFrame emptyVarMap 
                        (res,_) <- evalWithEnv (withScope vmr f)
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
