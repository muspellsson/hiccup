module Hiccup (runTcl, mkInterp,runInterp) where

import Control.Monad.State
import qualified Data.Map as Map
import Control.Arrow
import System.IO
import Control.Monad.Error
import System.Exit
import Data.IORef
import Data.Char (toLower,toUpper)
import Data.List (intersperse)
import System.Environment
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import BSParse
import qualified TclObj as T

type BString = B.ByteString

data Err = ERet !RetVal | EBreak | EContinue | EDie String deriving (Eq)

instance Error Err where
 noMsg    = EDie "An error occurred."
 strMsg s = EDie s

data TclEnv = TclEnv { vars :: VarMap, procs :: ProcMap, upMap :: Map.Map BString (Int,BString) } 
type TclM = ErrorT Err (StateT [TclEnv] IO)
type TclProc = [T.TclObj] -> TclM RetVal
type ProcMap = Map.Map BString TclProc
type VarMap = Map.Map BString T.TclObj

type RetVal = T.TclObj -- IGNORE

procMap :: ProcMap
procMap = Map.fromList . map (B.pack *** id) $
 [("proc",procProc),("set",procSet),("upvar",procUpVar),("puts",procPuts),("gets",procGets),
  ("uplevel", procUpLevel),("if",procIf),("while",procWhile),("eval", procEval),("exit",procExit),
  ("list",procList),("lindex",procLindex),("llength",procLlength),("return",procReturn),
  ("break",procRetv EBreak),("catch",procCatch),("continue",procRetv EContinue),("eq",procEq),("ne",procNe),
  ("==",procEql),
  ("string", procString), ("append", procAppend), ("info", procInfo), ("global", procGlobal), ("source", procSource), ("incr", procIncr)]
   ++ map (id *** procMath) [("+",(+)), ("*",(*)), ("-",(-)), ("/",div), ("<", toI (<)),("<=",toI (<=)),("!=",toI (/=))]

io :: IO a -> TclM a
io = liftIO

toI :: (Int -> Int -> Bool) -> (Int -> Int -> Int)
toI n a b = if n a b then 1 else 0

baseEnv = TclEnv { vars = Map.empty, procs = procMap, upMap = Map.empty }

data Interpreter = Interpreter (IORef [TclEnv])
mkInterp :: IO Interpreter
mkInterp = newIORef [baseEnv] >>= return . Interpreter

runInterp :: Interpreter -> BString -> IO (Either BString BString)
runInterp (Interpreter i) s = do
                 bEnv <- readIORef i
                 (r,i') <- runStateT (runErrorT ((doTcl s))) bEnv 
                 writeIORef i i'
                 return (fixErr r)
  where perr (EDie s) = B.pack s
        perr (ERet v) = T.asBStr v
        perr EBreak   = B.pack $ "invoked \"break\" outside of a loop"
        perr EContinue   = B.pack $ "invoked \"continue\" outside of a loop"
        fixErr (Left x) = Left (perr x)
        fixErr (Right v) = Right (T.asBStr v)

runTcl v = mkInterp >>= (`runInterp` v)

ret = return T.empty

doTcl :: BString -> TclM RetVal
doTcl s = runCmds =<< getParsed s

runCmds = liftM last . mapM runCommand

getParsed str = do p <- T.asParsed str
                   return $ filter (not . null) p

(.==) bs str = (T.asBStr bs) == B.pack str
{-# INLINE (.==) #-}

doCond :: T.TclObj -> TclM Bool
doCond str = do 
      p <- getParsed str
      case p of
        [x]      -> runCommand x >>= return . T.asBool
        _       -> tclErr "Too many statements in conditional"

trim :: T.TclObj -> BString
trim = B.reverse . dropWhite . B.reverse . dropWhite . T.asBStr

withScope :: TclM RetVal -> TclM RetVal
withScope f = do
  (o:old) <- get
  put $ (baseEnv { procs = procs o }) : o : old
  f `ensure` (get >>= put . tail)

ensure :: TclM RetVal -> TclM () -> TclM RetVal
ensure action p = do
   r <- action `catchError` (\e -> p >> throwError e)
   p
   return r

set :: BString -> T.TclObj -> TclM ()
set str v = do when (B.null str) $ tclErr "Empty varname to set!" 
               env <- liftM head get
               case upped str env of
                 Just (i,s) -> uplevel i (set s v)
                 Nothing -> do es <- liftM tail get
                               put ((env { vars = Map.insert str v (vars env) }):es)

upped s e = Map.lookup s (upMap e)

getProc str = get >>= return . Map.lookup str . procs . head
regProc name pr = modify (\(x:xs) -> (x { procs = Map.insert name pr (procs x) }):xs) >> ret

evalw :: TclWord -> TclM RetVal
evalw (Word s)      = interp s
evalw (NoSub s (Just (p,_)))     = return $ T.mkTclBStrP s (Just p)
evalw (NoSub s Nothing)     = return $ T.mkTclBStrP s Nothing
evalw (Subcommand c) = runCommand c

ptrace = True -- IGNORE

runCommand :: [TclWord] -> TclM RetVal
runCommand [Subcommand s] = runCommand s
runCommand args = do 
 (name:evArgs) <- mapM evalw args
 -- (e:_) <- get 
 -- when ptrace $ io (print (name,args) >> print (vars e)) -- IGNORE
 proc <- getProc (T.asBStr name)
 maybe (tclErr ("invalid command name: " ++ (T.asStr name))) ($! evArgs) proc

procProc, procSet, procPuts, procIf, procWhile, procReturn, procUpLevel :: TclProc
procSet [s1,s2] = set (T.asBStr s1) s2 >> return s2
procSet [s1] = varGet (T.asBStr s1)
procSet _    = tclErr $ "set: Wrong arg count"

procPuts s@(sh:st) = (io . mapM_ puts) (map T.asBStr txt) >> ret
 where (puts,txt) = if sh .== "-nonewline" then (B.putStr,st) else (B.putStrLn,s)
procPuts x         = tclErr $ "Bad args to puts: " ++ show x

procGets args = case args of
          [ch] -> getChan (T.asBStr ch) >>= checkReadable >>= io . B.hGetLine >>= treturn
          [ch,vname] -> do h <- getChan (T.asBStr ch) >>= checkReadable
                           eof <- io (hIsEOF h)
                           if eof
                             then set (T.asBStr vname) (T.empty) >> return (T.mkTclInt (-1))
                             else do s <- io (B.hGetLine h)
                                     set (T.asBStr vname) (T.mkTclBStr s)
                                     return $ T.mkTclInt (B.length s)
          _  -> tclErr "gets: Wrong arg count"

checkReadable c = do r <- (io (hIsReadable c))
                     if r then return c else (tclErr $ "channel wasn't opened for reading")

getChan :: BString -> TclM Handle
getChan c = maybe (tclErr ("cannot find channel named " ++ show c)) return (lookup c chans)
 where chans :: [(BString,Handle)]
       chans = zip (map B.pack ["stdin", "stdout", "stderr"]) [stdin, stdout, stderr]

procEq [a,b] = return $ T.fromBool (a == b)
procNe [a,b] = return $ T.fromBool (a /= b)

procMath :: (Int -> Int -> Int) -> TclProc
procMath op [s1,s2] = liftM2 op (T.asInt s1) (T.asInt s2) >>= return . T.mkTclInt
procMath op _       = tclErr "math: Wrong arg count"

procEql [a,b] = case (T.asInt a, T.asInt b) of
                  (Just ia, Just ib) -> return $ T.fromBool (ia == ib)
                  _                  -> procEq [a,b]


tclEval s = procEval [T.mkTclBStr (B.pack s)] >>= return . T.asBStr

procEval [s] = doTcl (T.asBStr s)
procEval x   = tclErr $ "Bad eval args: " ++ show x

procSource [s] = io (B.readFile (T.asStr s)) >>= doTcl

procExit [] = io (exitWith ExitSuccess)

procCatch [s] = (doTcl (T.asBStr s) >> procReturn [T.tclFalse]) `catchError` (return . catchRes)
 where catchRes (EDie s) = T.tclTrue
       catchRes _        = T.tclFalse

retmod f = \v -> treturn (f `onObj` v)

onObj f o = (f (T.asBStr o))

procString :: TclProc
procString (f:s:args) 
 | f .== "trim" = treturn (trim s)
 | f .== "tolower" = retmod (B.map toLower) s
 | f .== "toupper" = retmod (B.map toUpper) s
 | f .== "length" = return $ T.mkTclInt (B.length `onObj` s)
 | f .== "index" = case args of 
                          [i] -> do ind <- T.asInt i
                                    if ind >= (B.length `onObj` s) || ind < 0 then ret else treturn $ B.singleton (B.index (T.asBStr s) ind)
                          _   -> tclErr $ "Bad args to string index."
 | otherwise            = tclErr $ "Can't do string action: " ++ show f

tclErr = throwError . EDie

procInfo [x] = if x .== "commands" 
                 then get >>= procList . toObs . Map.keys . procs . head
                 else if x .== "vars" 
                        then get >>= procList . toObs . Map.keys . vars . head
                        else tclErr $ "Unknown info command: " ++ show x

toObs = map T.mkTclBStr
procAppend (v:vx) = do val <- varGet (T.asBStr v) `catchError` \_ -> ret
                       procSet [v, oconcat (val:vx)]

oconcat :: [T.TclObj] -> T.TclObj
oconcat = T.mkTclBStr . B.concat . map T.asBStr

procList a = treturn $ (map (escape . T.asBStr) a) `joinWith` ' '
 where escape s = if B.elem ' ' s then B.concat [B.singleton '{', s, B.singleton '}'] else s

procLindex [l] = return l
procLindex [l,i] = do items <- liftM (map to_s . head) (getParsed l)
                      ind <- T.asInt i
                      if ind >= length items then ret else treturn (items !! ind)

to_s (Word s)  = s
to_s (NoSub s p) = s
to_s x         = error $ "to_s doesn't understand: " ++ show x

procIncr [vname] = incr vname 1
procIncr [vname,val] = T.asInt val >>= incr vname
procIncr _ = tclErr $ "Wrong number of args to incr"
incr n i =  do v <- varGet bsname
               intval <- T.asInt v
               let res = (T.mkTclInt (intval + i))
               set bsname res
               return res
 where bsname = T.asBStr n

procLlength [lst] 
  | B.null `onObj` lst = return T.tclFalse
  | otherwise = liftM (T.mkTclInt . length . head)  (getParsed lst)
procLlength x = tclErr $ "Bad args to llength: " ++ show x

procIf (cond:yes:rest) = do
  condVal <- doCond cond
  if condVal then doTcl (T.asBStr yes)
    else case rest of
          [s,blk] -> if s .== "else" then doTcl (T.asBStr blk) else tclErr "Invalid If"
          (s:r)   -> if s .== "elseif" then procIf r else tclErr "Invalid If"
          []      -> ret
procIf x = tclErr "Not enough arguments to If."

procWhile [cond,body] = loop `catchError` herr
 where herr EBreak    = ret
       herr (ERet s)  = return s
       herr EContinue = loop `catchError` herr
       herr x         = throwError x
       loop = do condVal <- doCond cond
                 pbody <- getParsed body
                 if condVal then runCmds pbody >> loop else ret

procReturn [s] = throwError (ERet s)
procRetv c [] = throwError c
procError [s] = tclErr (T.asStr s)

procUpLevel [p]    = uplevel 1 (procEval [p])
procUpLevel (si:p) = T.asInt si >>= \i -> uplevel i (procEval p)

uplevel i p = do 
  (curr,new) <- liftM (splitAt i) get 
  put new
  res <- p
  get >>= put . (curr ++)
  return res

procUpVar [d,s]    = upvar 1 d s
procUpVar [si,d,s] = T.asInt si >>= \i -> upvar i d s

procGlobal lst@(_:_) = mapM_ inner lst >> ret
 where inner g = do lst <- get
                    let len = length lst - 1
                    upvar len g g

upvar n d s = do (e:es) <- get 
                 put ((e { upMap = Map.insert (T.asBStr s) (n, (T.asBStr d)) (upMap e) }):es)
                 ret

procProc [name,args,body] = do
  params <- case parseArgs (T.asBStr args) of
              Nothing -> if trim args .== "" then return [] else tclErr $ "Parse failed: " ++ show (T.asBStr args)
              Just (r,_) -> return $ map to_s r
  pbody <- getParsed body
  regProc (T.asBStr name) (procRunner params pbody)

procProc x = tclErr $ "proc: Wrong arg count (" ++ show (length x) ++ "): " ++ show (map T.asBStr x)


procRunner :: [BString] -> [[TclWord]] -> [T.TclObj] -> TclM RetVal
procRunner pl body args = withScope $ do when invalidCount $ tclErr ("wrong # args: should be " ++ show (pl `joinWith` ' '))
                                         zipWithM_ set pl args
                                         when hasArgs $ do
                                               val <- procList (drop ((length pl) - 1) args) 
                                               set (B.pack "args") val
                                         (runCmds body) `catchError` herr
 where herr (ERet s) = return s
       herr (EDie s) = tclErr s
       invalidCount 
           | hasArgs   = length args < (length pl - 1)
           | otherwise = length args /= length pl
       hasArgs = (not . null) pl && (last pl .== "args")



joinWith bsl c = B.concat (intersperse (B.singleton c) bsl)

varGet :: BString -> TclM RetVal
varGet name = do env <- liftM head get
                 case upped name env of
                   Nothing -> maybe (tclErr ("can't read \"$" ++ T.asStr name ++ "\": no such variable")) 
                                    return 
                                    (Map.lookup name (vars env))
                   Just (i,n) -> uplevel i (varGet n)
                   
treturn = return . T.mkTclBStr 

interp :: BString -> TclM RetVal
interp str = case wrapInterp str of
                   Left s -> treturn s
                   Right x -> handle x
 where f (Word match) = varGet match
       f x            = runCommand [x]
       handle (b,m,a) = do mid <- f m
                           let front = B.append b (T.asBStr mid)
                           interp a >>= \v -> treturn (B.append front (T.asBStr v))
