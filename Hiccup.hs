module Hiccup (runTcl, mkInterp,runInterp,hiccupTests) where

import Control.Monad.State
import qualified Data.Map as Map
import Control.Arrow
import System.IO
import Control.Monad.Error
import System.Exit
import Data.IORef
import Data.Char (toLower,toUpper)
import Data.List (intersperse)
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import BSParse
import qualified TclObj as T
import Test.HUnit  -- IGNORE

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

coreProcs, baseProcs, mathProcs :: ProcMap

coreProcs = Map.fromList . map (B.pack *** id) $
 [("proc",procProc),("set",procSet),("upvar",procUpVar),
  ("uplevel", procUpLevel),("if",procIf),
  ("while",procWhile),("eval", procEval),("exit",procExit),
  ("list",procList),("return",procReturn),
  ("break",procRetv EBreak),("catch",procCatch),
  ("continue",procRetv EContinue),("eq",procEq),("ne",procNe),
  ("==",procEql), ("error", procError), ("info", procInfo), ("global", procGlobal)]

baseProcs = Map.fromList . map (B.pack *** id) $
 [("puts",procPuts),("gets",procGets),
  ("lindex",procLindex),("llength",procLlength),
  ("string", procString), ("append", procAppend), 
  ("source", procSource), ("incr", procIncr)]


mathProcs = Map.fromList . map (B.pack *** procMath) $ 
   [("+",(+)), ("*",(*)), ("-",(-)), 
    ("/",div), ("<", toI (<)),("<=",toI (<=)),("!=",toI (/=))]

io :: IO a -> TclM a
io = liftIO

toI :: (Int -> Int -> Bool) -> (Int -> Int -> Int)
toI n a b = if n a b then 1 else 0

baseEnv = TclEnv { vars = Map.empty, procs = procMap, upMap = Map.empty }
 where procMap = Map.unions [coreProcs, baseProcs, mathProcs]

data Interpreter = Interpreter (IORef [TclEnv])

mkInterp :: IO Interpreter
mkInterp = newIORef [baseEnv] >>= return . Interpreter

runInterp :: BString -> Interpreter -> IO (Either BString BString)
runInterp s = runInterp' (doTcl s)

runInterp' t (Interpreter i) = do
                 bEnv <- readIORef i
                 (r,i') <- runStateT (runErrorT (t)) bEnv 
                 writeIORef i i'
                 return (fixErr r)
  where perr (EDie s) = B.pack s
        perr (ERet v) = T.asBStr v
        perr EBreak   = B.pack $ "invoked \"break\" outside of a loop"
        perr EContinue   = B.pack $ "invoked \"continue\" outside of a loop"
        fixErr (Left x) = Left (perr x)
        fixErr (Right v) = Right (T.asBStr v)

runTcl v = mkInterp >>= runInterp v

ret = return T.empty

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
        _        -> tclErr "Too many statements in conditional"

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
evalw (Word s)               = interp s
evalw (NoSub s (Just (p,_))) = return $ T.mkTclBStrP s (Just p)
evalw (NoSub s Nothing)      = return $ T.mkTclBStrP s Nothing
evalw (Subcommand c)         = runCommand c

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
procSet args = case args of
     [s1,s2] -> set (T.asBStr s1) s2 >> return s2
     [s1]    -> varGet (T.asBStr s1)
     _       -> argErr "set"

procPuts args = case args of
                 [s] -> tPutLn stdout s
                 [a1,str] -> if a1 .== "-nonewline" then tPut stdout str
                               else do h <- getChan (T.asBStr a1) >>= checkWritable
                                       tPutLn h str
                 [a1,a2,str] ->do unless (a1 .== "-nonewline") bad
                                  h <- getChan (T.asBStr a2) >>= checkWritable
                                  tPut h str 
                 _        -> bad
 where tPut h s = (io . B.hPutStr h . T.asBStr) s >> ret
       tPutLn h s = (io . B.hPutStrLn h . T.asBStr) s >> ret
       bad = argErr "puts"

procGets args = case args of
          [ch] -> getChan (T.asBStr ch) >>= checkReadable >>= io . B.hGetLine >>= treturn
          [ch,vname] -> do h <- getChan (T.asBStr ch) >>= checkReadable
                           eof <- io (hIsEOF h)
                           if eof
                             then set (T.asBStr vname) (T.empty) >> return (T.mkTclInt (-1))
                             else do s <- io (B.hGetLine h)
                                     set (T.asBStr vname) (T.mkTclBStr s)
                                     return $ T.mkTclInt (B.length s)
          _  -> argErr "gets"


checkReadable c = do r <- io (hIsReadable c)
                     if r then return c else (tclErr $ "channel wasn't opened for reading")

checkWritable c = do r <- io (hIsWritable c)
                     if r then return c else (tclErr $ "channel wasn't opened for writing")

getChan :: BString -> TclM Handle
getChan c = maybe (tclErr ("cannot find channel named " ++ show c)) return (lookup c chans)
 where chans :: [(BString,Handle)]
       chans = zip (map B.pack ["stdin", "stdout", "stderr"]) [stdin, stdout, stderr]

procEq args = case args of
                  [a,b] -> return $ T.fromBool (a == b)
                  _     -> argErr "eq"

procNe args = case args of
                  [a,b] -> return $ T.fromBool (a /= b)
                  _     -> argErr "ne"

argErr s = tclErr ("wrong # of args: " ++ s)


procMath :: (Int -> Int -> Int) -> TclProc
procMath op [s1,s2] = liftM2 op (T.asInt s1) (T.asInt s2) >>= return . T.mkTclInt
procMath _ _       = argErr "math"

procEql [a,b] = case (T.asInt a, T.asInt b) of
                  (Just ia, Just ib) -> return $ T.fromBool (ia == ib)
                  _                  -> procEq [a,b]
procEql _ = argErr "=="


procEval [s] = doTcl s
procEval x   = tclErr $ "Bad eval args: " ++ show x

procSource args = case args of
                  [s] -> io (B.readFile (T.asStr s)) >>= doTcl
                  _   -> argErr "source"

procExit args = case args of
            [] -> io (exitWith ExitSuccess)
            [i] -> do v <- T.asInt i
                      let ecode = if v == 0 then ExitSuccess else ExitFailure v
                      io (exitWith ecode)
            _  -> argErr "exit"

procCatch args = case args of
           [s] -> (doTcl s >> procReturn [T.tclFalse]) `catchError` (return . catchRes)
           _   -> argErr "catch"
 where catchRes (EDie _) = T.tclTrue
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
procString _ = tclErr $ "Bad string args"

tclErr = throwError . EDie

procInfo [x] = if x .== "commands" 
                    then get >>= procList . toObs . Map.keys . procs . head
                    else if x .== "vars" 
                          then get >>= procList . toObs . Map.keys . vars . head
                          else tclErr $ "Unknown info command: " ++ show x
procInfo _     = tclErr $ "info: Bad arg count"

toObs = map T.mkTclBStr

procAppend args = case args of
            (v:vx) -> do val <- varGet (T.asBStr v) `catchError` \_ -> ret
                         procSet [v, oconcat (val:vx)]
            _  -> argErr "append"
 where oconcat = T.mkTclBStr . B.concat . map T.asBStr

procList a = treturn $ (map (escape . T.asBStr) a) `joinWith` ' '
 where escape s = if B.elem ' ' s then B.concat [B.singleton '{', s, B.singleton '}'] else s

procLindex args = case args of
          [l]   -> return l
          [l,i] -> do items <- liftM (map to_s . head) (getParsed l)
                      ind <- T.asInt i
                      if ind >= length items then ret else treturn (items !! ind)
          _     -> argErr "lindex"

to_s (Word s)  = s
to_s (NoSub s _) = s
to_s x         = error $ "to_s doesn't understand: " ++ show x

procIncr [vname] = incr vname 1
procIncr [vname,val] = T.asInt val >>= incr vname
procIncr _ = argErr "incr"
incr n i =  do v <- varGet bsname
               intval <- T.asInt v
               let res = (T.mkTclInt (intval + i))
               set bsname res
               return res
 where bsname = T.asBStr n

procLlength args = case args of
        [lst] -> if B.null `onObj` lst 
                        then return T.tclFalse
                        else liftM (T.mkTclInt . length . head) (getParsed lst) 
        _     -> argErr "llength"

procIf (cond:yes:rest) = do
  condVal <- doCond cond
  if condVal then doTcl yes
    else case rest of
          [s,blk] -> if s .== "else" then doTcl blk else tclErr "Invalid If"
          (s:r)   -> if s .== "elseif" then procIf r else tclErr "Invalid If"
          []      -> ret
procIf _ = tclErr "Not enough arguments to If."

procWhile [cond,body] = loop `catchError` herr
 where herr EBreak    = ret
       herr (ERet s)  = return s
       herr EContinue = loop `catchError` herr
       herr x         = throwError x
       loop = do condVal <- doCond cond
                 pbody <- getParsed body
                 if condVal then runCmds pbody >> loop else ret

procWhile _ = argErr "while"

procReturn args = case args of
      [s] -> throwError (ERet s)
      []  -> throwError (ERet T.empty)
      _   -> argErr "return"

procRetv c [] = throwError c
procRetv c _  = argErr $ st c
 where st EContinue = "continue"
       st EBreak    = "break"
       st _         = "??"

procError [s] = tclErr (T.asStr s)
procError _ = argErr "error"

procUpLevel args = case args of
              [p]    -> uplevel 1 (procEval [p])
              (si:p) -> T.asInt si >>= \i -> uplevel i (procEval p)
              _      -> argErr "uplevel"

uplevel i p = do 
  (curr,new) <- liftM (splitAt i) get 
  put new
  res <- p
  get >>= put . (curr ++)
  return res

procUpVar args = case args of
     [d,s]    -> upvar 1 d s
     [si,d,s] -> T.asInt si >>= \i -> upvar i d s
     _        -> argErr "upvar"

procGlobal lst@(_:_) = mapM_ inner lst >> ret
 where inner g = do lst <- get
                    let len = length lst - 1
                    upvar len g g
procGlobal _         = argErr "global"

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
 where herr (ERet s)  = return s
       herr (EDie s)  = tclErr s
       herr EBreak    = tclErr "invoked \"break\" outside of a loop"
       herr EContinue = tclErr "invoked \"continue\" outside of a loop"
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

-- # TESTS # --


run :: TclM RetVal -> Either Err RetVal -> IO Bool
run t v = do ret <- liftM fst (runStateT (runErrorT (t)) [baseEnv])
             return (ret == v)

testProcEq = TestList [
      "1 eq 1 -> t" ~:          isTrue (procEq [int 1, int 1])
      ,"1 == 1 -> t" ~:         isTrue (procEql [int 1, int 1])
      ,"' 1 ' == 1 -> t" ~:     procEql [str " 1 ", int 1] `is` True
      ,"' 1 ' eq 1 -> f" ~:     procEq [str " 1 ", int 1] `is` False
      ,"' 1 ' eq ' 1 ' -> t" ~: procEq [str " 1 ", str " 1 "] `is` True
      ,"' 1 ' ne '1' -> t" ~: procNe [str " 1 ", str "1"] `is` True
   ]
 where (?=?) a b = assert (run b (Right a))
       isTrue c = T.tclTrue ?=? c
       isFalse c = T.tclFalse ?=? c
       is c b = (T.fromBool b) ?=? c
       int i = T.mkTclInt i
       str s = T.mkTclStr s


hiccupTests = TestList [ testProcEq ]

runUnit = runTestTT hiccupTests

-- # ENDTESTS # --
