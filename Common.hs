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

data TclEnv = TclEnv { vars :: VarMap, procs :: ProcMap, upMap :: Map.Map BString (Int,BString) } 
type TclM = ErrorT Err (StateT TclState IO)
data TclState = TclState { tclChans :: MVar ChanMap, tclStack :: [TclEnv] }
type TclProc = [T.TclObj] -> TclM RetVal
type ProcMap = Map.Map BString TclProc
type VarMap = Map.Map BString (Either T.TclObj TclArray)
type ChanMap = Map.Map BString T.TclChan

type TclArray = Map.Map BString T.TclObj

makeProcMap = Map.fromList . mapFst B.pack
makeState chans envl = do cm <- newMVar chans
                          return (TclState cm envl)

mapSnd f = map (\(a,b) -> (a, f b))
mapFst f = map (\(a,b) -> (f a, b))

getStack = gets tclStack -- TODO: Guard for empty stack here?
putStack s = modify (\v -> v { tclStack = s })
modStack :: ([TclEnv] -> [TclEnv]) -> TclM ()
modStack f = getStack >>= putStack . f
getFrame = do st <- getStack  
              case st of
                []    -> tclErr "Aack. Tried to go up too far in the stack."
                (x:_) -> return x

{-# INLINE getStack  #-}
{-# INLINE putStack  #-}

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

getProcRaw :: BString -> TclM TclProc
getProcRaw str = eatGlobal str >>= getProc

eatGlobal str = case parseNS str of 
                   Left str    -> return str
                   Right [a,b] -> if B.null a then return b else nsError
                   _           -> nsError
 where nsError = tclErr $ "can't lookup " ++ show str ++ ", namespaces not yet supported"

getProc :: BString -> TclM TclProc
getProc str = getFrame >>= getProc' str 


getProc' str e = let pr = procs e 
                 in case Map.lookup str pr of
                       Nothing -> tclErr ("invalid command name " ++ show str)
                       Just v  -> return v


rmProc :: BString -> TclM ()
rmProc name = modStack (\(x:xs) -> (x { procs = Map.delete name (procs x) }):xs)

regProc :: BString -> TclProc -> TclM RetVal
regProc name pr = modStack (\(x:xs) -> (x { procs = Map.insert name pr (procs x) }):xs) >> ret

varSet :: BString -> T.TclObj -> TclM ()
varSet n v = case parseArrRef n of
              Nothing -> varSet2 n Nothing v
              Just (an,i) -> varSet2 an (Just i) v

varSetVal :: BString -> T.TclObj -> TclM ()
varSetVal n v = varSet2 n Nothing v 
{-# INLINE varSetVal #-}

varSet2 :: BString -> Maybe BString -> T.TclObj -> TclM ()
varSet2 str ind v = do 
     when (B.null str) (tclErr "Empty varname to set!")
     (env:es) <- getStack
     case upped str env of
         Just (i,s) -> uplevel i (varSet2 s ind v)
         Nothing    -> modEnv env >>= \ne -> putStack (ne:es)
 where modEnv env = do
                let ev = vars env 
                case ind of
                  Nothing -> return (env { vars = Map.insert str (Left v) ev })
                  Just i  -> case Map.findWithDefault (Right Map.empty) str ev of
                               Left _ -> tclErr $ "Can't set \"" ++ B.unpack str ++ "(" ++ B.unpack i ++ ")\": variable isn't array"
                               Right prev ->  return (env { vars = Map.insert str (Right (Map.insert i v prev)) ev })

varExists :: BString -> TclM Bool
varExists name = do
  env <- getFrame
  case upped name env of
     Nothing    -> return $ maybe False (const True) (Map.lookup name (vars env))
     Just (_,_) -> return True -- TODO: Don't assume an upref is always correct?

varRename :: BString -> BString -> TclM RetVal
varRename old new = do
  pr <- getProcRaw old
  rmProc old  
  if (not (B.null new)) then regProc new pr else ret

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


varGet :: BString -> TclM RetVal
varGet n = case parseArrRef n of
              Nothing -> varGet2 n Nothing
              Just (an,i) -> varGet2 an (Just i)

getArray :: BString -> TclM TclArray
getArray name = do
   env <- getFrame
   case upped name env of
      Nothing    -> do val <- Map.lookup name (vars env) `ifNothing` ("can't read \"$" ++ B.unpack name ++ "\": no such variable")
                       case val of
                          (Right m)  -> return m
                          (Left _)   -> tclErr $ "can't read " ++ show name ++ ": variable isn't array"
      Just (i,n) -> uplevel i (getArray n)

varGet2 :: BString -> Maybe BString -> TclM RetVal
varGet2 name ind = do 
   env <- getFrame
   case upped name env of
      Nothing    -> do val <- Map.lookup name (vars env) `ifNothing` ("can't read \"$" ++  B.unpack name ++ "\": no such variable")
                       case (val, ind) of
                          (Left o, Nothing)  -> return o
                          (Right _, Nothing) -> tclErr $ "can't read \"" ++ B.unpack name ++ "\": variable is array"
                          (Left _, Just _)   -> tclErr $ "can't read \"" ++ B.unpack name ++ "\": variable isn't array"
                          (Right m, Just i)  -> Map.lookup i m `ifNothing` ("can't read \"" ++ B.unpack i ++ "\": no such element in array")
      Just (i,n) -> uplevel i (varGet2 n ind)

uplevel :: Int -> TclM a -> TclM a
uplevel i p = do 
  (curr,new) <- liftM (splitAt i) getStack
  putStack new
  res <- p
  modStack (curr ++)
  return res
{-# INLINE uplevel #-}

upvar n d s = do (e:es) <- getStack
                 putStack ((e { upMap = Map.insert (T.asBStr s) (n, (T.asBStr d)) (upMap e) }):es)
                 ret
{-# INLINE upvar #-}

withScope :: TclM RetVal -> TclM RetVal
withScope f = do
  (o:old) <- getStack
  putStack $ (emptyEnv { procs = procs o }) : o : old
  f `ensure` (modStack (drop 1))


ifFails f v = f `orElse` (return v)

orElse f f2 = f `catchError` (\_ -> f2)

ensure :: TclM RetVal -> TclM () -> TclM RetVal
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
(.==) bs str = (T.asBStr bs) == B.pack str
{-# INLINE (.==) #-}

emptyEnv = TclEnv { vars = Map.empty, procs = Map.empty, upMap = Map.empty }


-- # TESTS # --

runWithEnv :: [TclEnv] -> TclM RetVal -> Either Err RetVal -> IO Bool
runWithEnv env t v = 
  do st <- makeState Map.empty env
     retv <- liftM fst (runTclM t st)
     return (retv == v)

errWithEnv :: [TclEnv] -> TclM a -> IO (Either Err a)
errWithEnv env t = 
    do st <- makeState Map.empty env
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
   where (?=>) a b@(b1,b2) = (a ++ " -> " ++ show b) ~: parseArrRef (B.pack a) ~=? Just (B.pack b1, B.pack b2)
         should_be x r =  (x ++ " should be " ++ show r) ~: parseArrRef (B.pack x) ~=? r

  b = B.pack

  evalWithEnv :: [TclEnv] -> TclM a -> IO (Either Err a, [TclEnv])
  evalWithEnv env t = 
    do st <- makeState Map.empty env
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
       "empty name" ~: (varSet (b "") (int 4)) `checkErr` "Empty varname to set!"
       ,"set exists" ~: (varSet (b "x") (int 1)) `checkExists` "x"
       ,"set exists2" ~: (varSet (b "boogie") (int 1)) `checkExists` "boogie"
       ,"checkeq" ~: checkEq (varSet name value) "varname" value
     ]

  getTests = TestList [
       "non-exist" ~: (varGet (b "boo")) `checkErr` "can't read \"$boo\": no such variable"
       ,"no err if exists" ~: checkNoErr ((varSet name value) >> varGet name)
     ]

  unsetTests = TestList [
       "non-exist" ~: (varUnset (b "boo")) `checkErr` "can't unset \"boo\": no such variable"
     ]

-- # ENDTESTS # --
