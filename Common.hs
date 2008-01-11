module Common where

import qualified Data.ByteString.Char8 as B
import Control.Monad.Error
import qualified TclObj as T
import Control.Concurrent.MVar
import Control.Monad.State
import qualified Data.Map as Map
import Data.List (intersperse)
import Test.HUnit 

type BString = B.ByteString
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
type VarMap = Map.Map BString (Either T.TclObj (Map.Map BString T.TclObj))
type ChanMap = Map.Map BString T.TclChan

makeProcMap = Map.fromList . mapFst B.pack
makeState chans envl = do cm <- newMVar chans
                          return (TclState cm envl)

mapSnd f = map (\(a,b) -> (a, f b))
mapFst f = map (\(a,b) -> (f a, b))

getStack = gets tclStack
putStack s = modify (\v -> v { tclStack = s })
modStack :: ([TclEnv] -> [TclEnv]) -> TclM ()
modStack f = getStack >>= \s -> let r = f s in r `seq` putStack r
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

varSet :: BString -> T.TclObj -> TclM ()
varSet str v = do when (B.null str) $ tclErr "Empty varname to set!" 
                  (env:es) <- getStack
                  case upped str env of
                    Just (i,s) -> uplevel i (varSet s v)
                    Nothing    -> putStack ((env { vars = Map.insert str (Left v) (vars env) }):es)

varExists :: BString -> TclM Bool
varExists name = do
  env <- getFrame
  case upped name env of
     Nothing    -> return $ maybe False (const True) (Map.lookup name (vars env))
     Just (i,_) -> return True

varUnset :: BString -> TclM RetVal
varUnset name = do 
   (env:es) <- getStack
   case upped name env of
      Nothing    -> do let vmap = vars env 
                       verifyNameIn vmap
                       putStack ((env { vars = Map.delete name vmap }):es)
      Just (i,s) -> do uplevel i (varUnset s)
                       let umap = upMap env
                       verifyNameIn umap
                       (_:es2) <- getStack
                       putStack ((env { upMap = Map.delete name umap }):es2)
   ret
 where bad = tclErr ("can't unset " ++ show name ++ ": no such variable")
       verifyNameIn m = unless (Map.member name m) bad

varDump = do env <- getStack
             io (putStrLn "")
             io (mapM_ (\(i,a,b) -> (putStrLn (i ++ ":") >> putStrLn a >> putStrLn b))  (map fixit (zip [0..] env)))
  where fixit (i,s) = (show i, Map.showTree (vars s), Map.showTree (upMap s))


varGet :: BString -> TclM RetVal
varGet name = do env <- getFrame
                 case upped name env of
                   Nothing    -> do val <- Map.lookup name (vars env) `ifNothing` ("can't read \"$" ++ T.asStr name ++ "\": no such variable")
                                    case val of
                                      Left o -> return o
                                      Right _ -> tclErr "variable is array"
                   Just (i,n) -> uplevel i (varGet n)

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

withScope :: TclM RetVal -> TclM RetVal
withScope f = do
  (o:old) <- getStack
  putStack $ (emptyEnv { procs = procs o }) : o : old
  f `ensure` (modStack (drop 1))

ensure :: TclM RetVal -> TclM () -> TclM RetVal
ensure action p = do
   r <- action `catchError` (\e -> p >> throwError e)
   p
   return r

ifNothing m e = maybe (tclErr e) return m
{-# INLINE ifNothing #-}

ret :: TclM RetVal
ret = return T.empty

treturn :: BString -> TclM RetVal
treturn = return . T.mkTclBStr 
{-# INLINE treturn #-}

(.==) :: T.TclObj -> String -> Bool
(.==) bs str = (T.asBStr bs) == B.pack str
{-# INLINE (.==) #-}

emptyEnv = TclEnv { vars = Map.empty, procs = Map.empty, upMap = Map.empty }

joinWith bsl c = B.concat (intersperse (B.singleton c) bsl)

-- # TESTS # --

runWithEnv :: [TclEnv] -> TclM RetVal -> Either Err RetVal -> IO Bool
runWithEnv env t v = 
  do st <- makeState Map.empty env
     retv <- liftM fst (runTclM t st)
     return (retv == v)

commonTests = TestList [ setTests, getTests, unsetTests ] where

  b = B.pack

  evalWithEnv :: [TclEnv] -> TclM a -> IO (Either Err a, [TclEnv])
  evalWithEnv env t = 
    do st <- makeState Map.empty env
       (retv, resStack) <- runTclM t st
       return (retv, tclStack resStack)

  errWithEnv :: [TclEnv] -> TclM a -> IO (Either Err a)
  errWithEnv env t = 
    do st <- makeState Map.empty env
       retv <- liftM fst (runTclM t st)
       return retv

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
