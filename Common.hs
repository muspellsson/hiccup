{-# OPTIONS_GHC -fbang-patterns #-}
module Common (RetVal, TclM
       ,TclState
       ,Err(..)
       ,TclProc,TclProcT(..)
       ,runTclM
       ,makeState
       ,runWithEnv
       ,withScope
       ,makeProcMap      
       ,getProc
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
       ,commandNames
       ,commonTests
    ) where

import qualified Data.ByteString.Char8 as B
import Control.Monad.Error
import qualified TclObj as T
import Control.Monad.State
import qualified Data.Map as Map
import TclChan
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
         nsName :: String, 
         nsProcs :: ProcMap, 
         nsVars :: VarMap, 
         nsParent :: Maybe Namespace,
         nsChildren :: Map.Map BString Namespace } -- Ok.. and how do we use this?

globalNS = TclNS { nsName = "::", nsProcs = Map.empty, nsVars = Map.empty, nsParent = Nothing, nsChildren = Map.empty }

data TclProcT = TclProcT { procName :: BString, procBody :: BString,  procFunction :: TclProc }

data TclFrame = TclFrame { vars :: VarMap, upMap :: Map.Map BString (Int,BString) } 
type TclStack = [TclFrame]
data TclState = TclState { tclChans :: ChanMap, tclStack :: TclStack, tclProcs :: ProcMap }
type TclProc = [T.TclObj] -> TclM RetVal
type ProcMap = Map.Map BString TclProcT
type VarMap = Map.Map BString TclVar
data TclVar = ScalarVar T.TclObj | ArrayVar TclArray deriving (Eq,Show)

type TclArray = Map.Map BString T.TclObj

makeProcMap = Map.fromList . map toTclProcT . mapFst pack
toTclProcT (n,v) = (n, TclProcT n errStr v)
 where errStr = pack $ show n ++ " isn't a procedure"

makeState :: [(BString,T.TclObj)] -> ProcMap -> IO TclState
makeState = makeState' baseChans

makeState' :: ChanMap -> [(BString,T.TclObj)] -> ProcMap -> IO TclState
makeState' chans vlist procs = do let fr = emptyFrame { vars = Map.fromList (mapSnd ScalarVar vlist) }
                                  return (TclState chans [fr] procs)

stackLevel = getStack >>= return . pred . length
globalVars = upglobal localVars
localVars = getFrame >>= return . Map.keys . vars
currentVars = do f <- getFrame
                 return $ Map.keys (vars f) ++ Map.keys (upMap f)

commandNames = getProcMap >>= return . Map.keys

getStack = do s <- gets tclStack -- TODO: Guard for empty stack here?
              case s of
                []    -> tclErr "Aack. Tried to go up too far in the stack."
                v     -> return v
getProcMap = gets tclProcs
putProcMap p = modify (\v -> v { tclProcs = p })
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


getProc :: BString -> TclM (Maybe TclProcT)
getProc str = getProcMap >>= \m -> return (getProc' str m)

getProc' :: BString -> ProcMap -> Maybe TclProcT
getProc' str m = Map.lookup str m

rmProc :: BString -> TclM ()
rmProc name = getProcMap >>= putProcMap . Map.delete name

regProc :: BString -> BString -> TclProc -> TclM RetVal
regProc name body pr = (getProcMap >>= putProcMap . Map.insert name (TclProcT name body pr)) >> ret

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
                let ev  = vars env 
                let str = vnName vn 
                case vnInd vn of
                  Nothing -> case Map.lookup str ev of
                              Just (ArrayVar _) -> tclErr $ "can't set " ++ showVN vn ++ ": variable is array"
                              _                 -> return (env { vars = Map.insert str (ScalarVar v) ev })
                  Just i  -> case Map.findWithDefault (ArrayVar Map.empty) str ev of
                               ScalarVar _     -> tclErr $ "Can't set " ++ showVN vn ++ ": variable isn't array"
                               ArrayVar prev ->  return (env { vars = Map.insert str (ArrayVar (Map.insert i v prev)) ev })

varMod' :: VarName -> (T.TclObj -> TclM RetVal) -> TclM RetVal
varMod' vn@(VarName str ind) f = do 
     (env:es) <- getStack
     case upped str env of
         Just (i,s) -> uplevel i (varMod' (vn { vnName = s }) f)
         Nothing    -> do (ne,v) <- modEnv env 
                          putStack (ne:es)
                          return v
 where modEnv env = do
                let ev = vars env 
                let old = Map.lookup str ev
                case (old,ind) of
                  (Nothing,_) -> tclErr $ "no such variable: " ++ showVN vn
                  (Just os, Nothing) -> case os of
                                             ScalarVar val -> do
                                                    val2 <- f val 
                                                    return ((env { vars = Map.insert str (ScalarVar val2) ev }), val2)
                                             _ -> tclErr $ "can't read " ++ showVN vn ++ ", variable is array"
                  (Just oa, Just i)  -> case oa of
                                         ArrayVar prev -> do  
                                             p2 <- Map.lookup i prev
                                             v <- f p2
                                             return ((env { vars = Map.insert str (ArrayVar (Map.insert i v prev)) ev }), v)
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
  | otherwise   = tclErr "namespaces not fully supported"
{-# INLINE runInNS #-}

varUnset' :: BString -> TclM RetVal
varUnset' name = do 
   (env:es) <- getStack
   case upped name env of
      Nothing    -> do let vmap = vars env 
                       verifyNameIn vmap
                       putStack ((env { vars = Map.delete name vmap }):es)
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
  where fixit (i,s) = (show i, Map.showTree (vars s), Map.showTree (upMap s))



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
      Nothing    -> return (Map.lookup name (vars env))
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
withScope f = do
  (o:old) <- getStack
  putStack $ emptyFrame : o : old
  f `ensure` (modStack (drop 1))

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

emptyFrame = TclFrame { vars = Map.empty, upMap = Map.empty }

makeEnsemble name subs = top
  where top args = case args of
                   (x:xs) -> case Map.lookup (T.asBStr x) subMap of
                              Nothing -> tclErr $ "unknown subcommand " ++ show (T.asBStr x) ++ ": must be " ++ commaList "or" (map unpack (Map.keys subMap))
                              Just f  -> (procFunction f) xs
                   []  -> argErr $ " should be \"" ++ name ++ "\" subcommand ?arg ...?"
        subMap = makeProcMap subs

-- # TESTS # --

runWithEnv :: TclM RetVal -> Either Err RetVal -> IO Bool
runWithEnv t v = 
  do st <- makeState' Map.empty [] Map.empty
     retv <- liftM fst (runTclM t st)
     return (retv == v)

errWithEnv :: TclM a -> IO (Either Err a)
errWithEnv t = 
    do st <- makeState' Map.empty [] Map.empty
       retv <- liftM fst (runTclM t st)
       return retv

commonTests = TestList [ setTests, getTests, unsetTests ] where

  b = pack

  evalWithEnv :: TclM a -> IO (Either Err a, TclStack)
  evalWithEnv t = 
    do st <- makeState' Map.empty [] Map.empty
       (retv, resStack) <- runTclM t st
       return (retv, tclStack resStack)


  checkErr a s = errWithEnv a >>= \v -> assertEqual "err match" (Left (EDie s)) v
  checkNoErr a = errWithEnv a >>= \v -> assertBool "err match" (isRight v)

  checkExists a n = do (_,(v:_)) <- evalWithEnv a 
                       vExists n v

  vExists vn env = assert (Map.member (b vn) (vars env))

  checkEq :: TclM t -> String -> T.TclObj -> Assertion
  checkEq a n val = do (_,(v:_)) <- evalWithEnv a 
                       vEq n v val

  vEq vn env val = assert ((Map.lookup (b vn) (vars env)) == (Just (ScalarVar val)))

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

  getTests = TestList [
       "non-exist" ~: (varGet (b "boo")) `checkErr` "can't read \"boo\": no such variable"
       ,"no err if exists" ~: checkNoErr ((varSet name value) >> varGet name)
     ]

  unsetTests = TestList [
       "non-exist" ~: (varUnset (b "boo")) `checkErr` "can't unset \"boo\": no such variable"
     ]

-- # ENDTESTS # --
