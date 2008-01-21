module Common where

import qualified Data.ByteString.Char8 as B
import Control.Monad.Error
import qualified TclObj as T
import Control.Concurrent.MVar
import Control.Monad.State
import qualified Data.Map as Map
import qualified TclChan as T
import Test.HUnit 
import BSParse (parseArrRef,parseNS)
import Util


type RetVal = T.TclObj -- IGNORE

data Err = ERet !RetVal | EBreak | EContinue | EDie String deriving (Eq,Show)

instance Error Err where
 noMsg    = EDie "An error occurred."
 strMsg s = EDie s

type TclM = ErrorT Err (StateT TclState IO)

data Namespace = TclNS { nsName :: BString, nsProcs :: ProcMap, nsVars :: VarMap, nsChildren :: Map.Map BString Namespace }

data TclProcT = TclProcT { procName :: BString, procFunction :: TclProc }

data TclEnv = TclEnv { vars :: VarMap, upMap :: Map.Map BString (Int,BString) } 
data TclState = TclState { tclChans :: MVar ChanMap, tclStack :: [TclEnv], tclProcs :: ProcMap }
type TclProc = [T.TclObj] -> TclM RetVal
type ProcMap = Map.Map BString TclProcT
type VarMap = Map.Map BString (Either T.TclObj TclArray)
type ChanMap = Map.Map BString T.TclChan

type TclArray = Map.Map BString T.TclObj

makeProcMap = Map.fromList . map toTclProcT . mapFst pack
toTclProcT (n,v) = (n, TclProcT n v)
makeState chans envl procs = do cm <- newMVar chans
                                return (TclState cm envl procs)

mapSnd f = map (\(a,b) -> (a, f b))
mapFst f = map (\(a,b) -> (f a, b))

getStack = gets tclStack -- TODO: Guard for empty stack here?
getProcMap = gets tclProcs
putProcMap p = modify (\v -> v { tclProcs = p })
putStack s = modify (\v -> v { tclStack = s })
modStack :: ([TclEnv] -> [TclEnv]) -> TclM ()
modStack f = getStack >>= putStack . f
getFrame = do st <- getStack  
              case st of
                []    -> tclErr "Aack. Tried to go up too far in the stack."
                (x:_) -> return x

{-# INLINE getStack  #-}
{-# INLINE putStack  #-}
{-# INLINE getFrame  #-}

io :: IO a -> TclM a
io = liftIO

tclErr:: String -> TclM a
tclErr = throwError . EDie

argErr s = tclErr ("wrong # of args: " ++ s)

runTclM :: TclM a -> TclState -> IO (Either Err a, TclState)
runTclM code env = runStateT (runErrorT code) env

getChan n = do s <- gets tclChans
               m <- (io . readMVar) s
               return (Map.lookup n m)

addChan c = do s <- gets tclChans
               io (modMVar s (Map.insert (T.chanName c) c))

modMVar m f = do v <- takeMVar m
                 putMVar m (f v)

removeChan c = do s <- gets tclChans
                  io (modMVar s (Map.delete (T.chanName c)))

baseChans = Map.fromList (map (\c -> (T.chanName c, c)) T.tclStdChans )

upped s e = Map.lookup s (upMap e)
{-# INLINE upped #-}

getProcRaw str = eatGlobal str >>= getProc

eatGlobal str = case parseNS str of 
                   Left str    -> return str
                   Right [a,b] -> if B.null a then return b else nsError
                   _           -> nsError
 where nsError = tclErr $ "can't lookup " ++ show str ++ ", namespaces not yet supported"

getProc :: BString -> TclM (Maybe TclProcT)
getProc str = getProcMap >>= \m -> return (getProc' str m)


getProc' :: BString -> ProcMap -> Maybe TclProcT
getProc' str m = Map.lookup str m


rmProc :: BString -> TclM ()
rmProc name = getProcMap >>= putProcMap . Map.delete name

regProc :: BString -> TclProc -> TclM RetVal
regProc name pr = (getProcMap >>= putProcMap . Map.insert name (TclProcT name pr)) >> ret

varSet :: BString -> T.TclObj -> TclM RetVal
varSet n v = case parseArrRef n of
              Nothing -> varSet' n Nothing v
              Just (an,i) -> varSet' an (Just i) v

varSetVal :: BString -> T.TclObj -> TclM RetVal
varSetVal n v = varSet' n Nothing v 
{-# INLINE varSetVal #-}

varSet' :: BString -> Maybe BString -> T.TclObj -> TclM RetVal
varSet' str ind v = do 
     (env:es) <- getStack
     case upped str env of
         Just (i,s) -> uplevel i (varSet' s ind v)
         Nothing    -> do ne <- modEnv env 
                          putStack (ne:es)
                          return v
 where modEnv env = do
                let ev = vars env 
                case ind of
                  Nothing -> return (env { vars = Map.insert str (Left v) ev })
                  Just i  -> case Map.findWithDefault (Right Map.empty) str ev of
                               Left _     -> tclErr $ "Can't set \"" ++ unpack str ++ "(" ++ unpack i ++ ")\": variable isn't array"
                               Right prev ->  return (env { vars = Map.insert str (Right (Map.insert i v prev)) ev })

varMod' :: BString -> Maybe BString -> (T.TclObj -> TclM RetVal) -> TclM RetVal
varMod' str ind f = do 
     (env:es) <- getStack
     case upped str env of
         Just (i,s) -> uplevel i (varMod' s ind f)
         Nothing    -> do (ne,v) <- modEnv env 
                          putStack (ne:es)
                          return v
 where modEnv env = do
                let ev = vars env 
                let old = Map.lookup str ev
                case (old,ind) of
                  (Nothing,_) -> tclErr $ "no such variable: " ++ show str
                  (Just os, Nothing) -> case os of
                                             Right _ -> tclErr $ "can't read " ++ show str ++ ", variable is array"
                                             Left val -> do
                                                 val2 <- f val 
                                                 return ((env { vars = Map.insert str (Left val2) ev }), val2)
                  (Just oa, Just i)  -> case oa of
                                         Left _ -> tclErr $ "Can't set \"" ++ unpack str ++ "(" ++ unpack i ++ ")\": variable isn't array"
                                         Right prev -> do  
                                             p2 <- Map.lookup i prev
                                             v <- f p2
                                             return ((env { vars = Map.insert str (Right (Map.insert i v prev)) ev }), v)

{-# INLINE varMod' #-}
varModify :: BString -> (T.TclObj -> TclM T.TclObj) -> TclM RetVal
varModify n f = case parseArrRef n of
      Nothing     -> varMod' n Nothing f
      Just (an,i) -> varMod' an (Just i) f
{-# INLINE varModify #-}


varExists :: BString -> TclM Bool
varExists name = do
  env <- getFrame
  case upped name env of
     Nothing    -> return $ maybe False (const True) (Map.lookup name (vars env))
     Just (_,_) -> return True -- TODO: Don't assume an upref is always correct?

varRename :: BString -> BString -> TclM RetVal
varRename old new = do
  mpr <- getProcRaw old
  case mpr of
   Nothing -> tclErr $ "bad command " ++ show old
   Just pr -> do rmProc old  
                 if not (B.null new) then regProc new (procFunction pr) else ret

varUnset :: BString -> TclM RetVal
varUnset name = do 
   (env:es) <- getStack
   case upped name env of
      Nothing    -> do let vmap = vars env 
                       verifyNameIn vmap
                       putStack ((env { vars = Map.delete name vmap }):es)
      Just (i,s) -> do let umap = upMap env
                       verifyNameIn umap
                       putStack ((env { upMap = Map.delete name umap }):es)
                       uplevel i (varUnset s) >> return ()
   ret
 where bad = tclErr ("can't unset " ++ show name ++ ": no such variable")
       verifyNameIn m = unless (Map.member name m) bad

varDump = do env <- getStack
             io (putStrLn "")
             io (mapM_ (\(i,a,b) -> (putStrLn (i ++ ":") >> putStrLn a >> putStrLn b))  (map fixit (zip [0..] env)))
  where fixit (i,s) = (show i, Map.showTree (vars s), Map.showTree (upMap s))


varGetRaw :: BString -> TclM RetVal
varGetRaw n = case parseArrRef n of
               Nothing -> varGet' n Nothing
               Just (an,i) -> varGet' an (Just i)

getArray :: BString -> TclM TclArray
getArray name = do
   env <- getFrame
   case upped name env of
      Nothing    -> do val <- Map.lookup name (vars env) `ifNothing` ("can't read \"$" ++ unpack name ++ "\": no such variable")
                       case val of
                          (Right m)  -> return m
                          (Left _)   -> tclErr $ "can't read " ++ show name ++ ": variable isn't array"
      Just (i,n) -> uplevel i (getArray n)

varGet' :: BString -> Maybe BString -> TclM RetVal
varGet' name ind = do 
   env <- getFrame
   case upped name env of 
      Nothing    -> do val <- Map.lookup name (vars env) `ifNothing` ("can't read \"$" ++  unpack name ++ "\": no such variable")
                       case (val, ind) of
                          (Left o, Nothing)  -> return o
                          (Right _, Nothing) -> tclErr $ "can't read \"" ++ unpack name ++ "\": variable is array"
                          (Left _, Just _)   -> tclErr $ "can't read \"" ++ unpack name ++ "\": variable isn't array"
                          (Right m, Just i)  -> Map.lookup i m `ifNothing` ("can't read \"" ++ unpack i ++ "\": no such element in array")
      Just (i,n) -> uplevel i (varGet' n ind)

uplevel :: Int -> TclM a -> TclM a
uplevel i p = do 
  (curr,new) <- liftM (splitAt i) getStack
  putStack new
  res <- p `ensure` (modStack (curr ++))
  return res
{-# INLINE uplevel #-}

upvar n d s = do (e:es) <- getStack
                 putStack ((e { upMap = Map.insert (T.asBStr s) (n, (T.asBStr d)) (upMap e) }):es)
                 ret
{-# INLINE upvar #-}

withScope :: TclM RetVal -> TclM RetVal
withScope f = do
  (o:old) <- getStack
  putStack $ emptyEnv : o : old
  f `ensure` (modStack (drop 1))


ifFails f v = f `orElse` (return v)

orElse f f2 = f `catchError` (\_ -> f2)

ensure action p = do
   r <- action `catchError` (\e -> p >> throwError e)
   p
   return r

ifNothing m e = maybe (tclErr e) return m
{-# INLINE ifNothing #-}

ret :: TclM RetVal
ret = return T.empty
{-# INLINE ret #-}

treturn :: BString -> TclM RetVal
treturn = return . T.mkTclBStr 
{-# INLINE treturn #-}

(.==) :: T.TclObj -> String -> Bool
(.==) bs str = (T.asBStr bs) == pack str
{-# INLINE (.==) #-}

emptyEnv = TclEnv { vars = Map.empty, upMap = Map.empty }


-- # TESTS # --

runWithEnv :: [TclEnv] -> TclM RetVal -> Either Err RetVal -> IO Bool
runWithEnv env t v = 
  do st <- makeState Map.empty env Map.empty
     retv <- liftM fst (runTclM t st)
     return (retv == v)

errWithEnv :: [TclEnv] -> TclM a -> IO (Either Err a)
errWithEnv env t = 
    do st <- makeState Map.empty env Map.empty
       retv <- liftM fst (runTclM t st)
       return retv

emptyEval = errWithEnv [emptyEnv]

commonTests = TestList [ setTests, getTests, unsetTests, testArr ] where
  testArr = TestList [
     "december" `should_be` Nothing
     ,"dec(mber" `should_be` Nothing
     ,"dec)mber" `should_be` Nothing
     ,"(cujo)" `should_be` Nothing
     ,"de(c)mber" `should_be` Nothing
     ,"a(1)"          ?=> ("a","1")
     ,"boo(4)"        ?=> ("boo","4")
     ,"xx(september)" ?=> ("xx","september")
     ,"arr(3,4,5)"    ?=> ("arr","3,4,5")
     ,"arr()"         ?=> ("arr","")
   ]
   where (?=>) a b@(b1,b2) = (a ++ " -> " ++ show b) ~: parseArrRef (bp a) ~=? Just (bp b1, bp b2)
         should_be x r =  (x ++ " should be " ++ show r) ~: parseArrRef (bp x) ~=? r

  b = B.pack
  bp = B.pack

  evalWithEnv :: [TclEnv] -> TclM a -> IO (Either Err a, [TclEnv])
  evalWithEnv env t = 
    do st <- makeState Map.empty env Map.empty
       (retv, resStack) <- runTclM t st
       return (retv, tclStack resStack)


  checkErr a s = errWithEnv [emptyEnv] a >>= \v -> assertEqual "err match" v (Left (EDie s))
  checkNoErr a = errWithEnv [emptyEnv] a >>= \v -> assertBool "err match" (isRight v)

  checkExists a n = do (_,(v:_)) <- evalWithEnv [emptyEnv] a 
                       vExists n v

  vExists vn env = assert (Map.member (b vn) (vars env))

  checkEq :: TclM t -> String -> T.TclObj -> Assertion
  checkEq a n val = do (_,(v:_)) <- evalWithEnv [emptyEnv] a 
                       vEq n v val

  vEq vn env val = assert ((Map.lookup (b vn) (vars env)) == (Just (Left val)))

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
       "non-exist" ~: (varGetRaw (b "boo")) `checkErr` "can't read \"$boo\": no such variable"
       ,"no err if exists" ~: checkNoErr ((varSet name value) >> varGetRaw name)
     ]

  unsetTests = TestList [
       "non-exist" ~: (varUnset (b "boo")) `checkErr` "can't unset \"boo\": no such variable"
     ]

-- # ENDTESTS # --
