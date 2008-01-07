module Hiccup (runTcl, mkInterp, runInterp, hiccupTests) where

--import Control.Monad.State
import qualified Data.Map as Map
import Control.Arrow
import System.IO
import qualified System.IO.Error as IOE 
import Control.Monad.Error
import System.Exit
import Data.IORef
import Data.Char (toLower,toUpper)
import Data.List (intersperse)
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import BSParse (TclWord(..), wrapInterp)
import qualified TclObj as T
import Test.HUnit  -- IGNORE

import Common

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
  ("source", procSource), ("incr", procIncr),
  ("open", procOpen), ("close", procClose)]


mathProcs = Map.fromList . map (B.pack *** procMath) $ 
   [("+",(+)), ("*",(*)), ("-",(-)), 
    ("/",div), ("<", toI (<)),(">", toI (>)),("<=",toI (<=)),("!=",toI (/=))]

toI :: (Int -> Int -> Bool) -> (Int -> Int -> Int)
toI n a b = if n a b then 1 else 0

baseEnv = TclEnv { vars = Map.empty, procs = procMap, upMap = Map.empty }
 where procMap = Map.unions [coreProcs, baseProcs, mathProcs]


data Interpreter = Interpreter (IORef TclState)

mkInterp :: IO Interpreter
mkInterp = do chans <- newIORef baseChans
              st <- newIORef (TclState chans [baseEnv]) 
              return (Interpreter st)

baseChans = Map.fromList (map (\x -> (T.chanName x, x)) T.tclStdChans )

runInterp :: BString -> Interpreter -> IO (Either BString BString)
runInterp s = runInterp' (evalTcl (T.mkTclBStr s))

runInterp' t (Interpreter i) = do
                 bEnv <- readIORef i
                 (r,i') <- runTclM t bEnv 
                 writeIORef i i'
                 return (fixErr r)
  where perr (EDie s)    = B.pack s
        perr (ERet v)    = T.asBStr v
        perr EBreak      = B.pack $ "invoked \"break\" outside of a loop"
        perr EContinue   = B.pack $ "invoked \"continue\" outside of a loop"
        fixErr (Left x)  = Left (perr x)
        fixErr (Right v) = Right (T.asBStr v)

runTcl v = mkInterp >>= runInterp v


evalTcl s = runCmds =<< getParsed s

runCmds = liftM last . mapM runCommand

getParsed :: T.TclObj -> TclM [[TclWord]]
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

withScope :: TclM RetVal -> TclM RetVal
withScope f = do
  (o:old) <- getStack
  putStack $ (baseEnv { procs = procs o }) : o : old
  f `ensure` (modStack tail)

ensure :: TclM RetVal -> TclM () -> TclM RetVal
ensure action p = do
   r <- action `catchError` (\e -> p >> throwError e)
   p
   return r


getProc str = getFrame >>= (`ifNothing` ("invalid command name " ++ show str)) . Map.lookup str . procs
regProc name pr = modStack (\(x:xs) -> (x { procs = Map.insert name pr (procs x) }):xs) >> ret

evalToken :: TclWord -> TclM RetVal
evalToken (Word s)               = interp s
evalToken (NoSub s res)          = return $ T.mkTclBStrP s res
evalToken (Subcommand c)         = runCommand c

runCommand :: [TclWord] -> TclM RetVal
runCommand args = do 
 (name:evArgs) <- mapM evalToken args
 proc <- getProc (T.asBStr name) 
 proc evArgs

procProc, procSet, procPuts, procIf, procWhile, procReturn, procUpLevel :: TclProc
procSet args = case args of
     [s1,s2] -> varSet (T.asBStr s1) s2 >> return s2
     [s1]    -> varGet (T.asBStr s1)
     _       -> argErr "set"

procPuts args = case args of
                 [s] -> tPutLn stdout s
                 [a1,str] -> if a1 .== "-nonewline" then tPut stdout str
                               else do h <- getWritable a1
                                       tPutLn h str
                 [a1,a2,str] ->do unless (a1 .== "-nonewline") bad
                                  h <- getWritable a2
                                  tPut h str 
                 _        -> bad
 where tPut h s = (io . B.hPutStr h . T.asBStr) s >> ret
       tPutLn h s = (io . B.hPutStrLn h . T.asBStr) s >> ret
       getWritable c = getChan (T.asBStr c) >>= checkWritable . T.chanHandle
       bad = argErr "puts"

procGets args = case args of
          [ch] -> do h <- getReadable ch
                     eof <- io (hIsEOF h)
                     if eof then ret else (io . B.hGetLine) h >>= treturn
          [ch,vname] -> do h <- getReadable ch
                           eof <- io (hIsEOF h)
                           if eof
                             then varSet (T.asBStr vname) (T.empty) >> return (T.mkTclInt (-1))
                             else do s <- io (B.hGetLine h)
                                     varSet (T.asBStr vname) (T.mkTclBStr s)
                                     return $ T.mkTclInt (B.length s)
          _  -> argErr "gets"
 where getReadable c = getChan (T.asBStr c) >>= checkReadable . T.chanHandle


checkReadable c = do r <- io (hIsReadable c)
                     if r then return c else (tclErr "channel wasn't opened for reading")

checkWritable c = do r <- io (hIsWritable c)
                     if r then return c else (tclErr "channel wasn't opened for writing")


getChan :: BString -> TclM T.TclChan
getChan c = do chans <- getChans
               Map.lookup c chans `ifNothing` ("cannot find channel named " ++ show c)

procEq args = case args of
                  [a,b] -> return $ T.fromBool (a == b)
                  _     -> argErr "eq"

procNe args = case args of
                  [a,b] -> return $ T.fromBool (a /= b)
                  _     -> argErr "ne"

procMath :: (Int -> Int -> Int) -> TclProc
procMath op [s1,s2] = liftM2 op (T.asInt s1) (T.asInt s2) >>= return . T.mkTclInt
procMath _ _       = argErr "math"

procEql [a,b] = case (T.asInt a, T.asInt b) of
                  (Just ia, Just ib) -> return $ T.fromBool (ia == ib)
                  _                  -> procEq [a,b]
procEql _ = argErr "=="


procEval [s] = evalTcl s
procEval x   = tclErr $ "Bad eval args: " ++ show x

procOpen args = case args of
         [fn] -> do eh <- io (IOE.try (openFile (T.asStr fn) ReadMode)) 
                    case eh of
                      Left e -> if IOE.isDoesNotExistError e 
                                      then tclErr $ "could not open " ++ show (T.asStr fn) ++ ": no such file or directory"
                                      else tclErr (show e)
                      Right h -> do
                          chan <- io (T.mkChan h)
                          addChan chan
                          treturn (T.chanName chan)
         _    -> argErr "open"

procClose args = case args of
         [ch] -> do h <- getChan (T.asBStr ch)
                    removeChan h
                    io (hClose (T.chanHandle h))
                    ret
         _    -> argErr "close"
                     

procSource args = case args of
                  [s] -> io (B.readFile (T.asStr s)) >>= evalTcl . T.mkTclBStr
                  _   -> argErr "source"

procExit args = case args of
            [] -> io (exitWith ExitSuccess)
            [i] -> do v <- T.asInt i
                      let ecode = if v == 0 then ExitSuccess else ExitFailure v
                      io (exitWith ecode)
            _  -> argErr "exit"

procCatch args = case args of
           [s] -> (procEval [s] >> procReturn [T.tclFalse]) `catchError` (return . catchRes)
           _   -> argErr "catch"
 where catchRes (EDie _) = T.tclTrue
       catchRes _        = T.tclFalse

retmod f = \v -> treturn (f `onObj` v)

onObj f o = (f (T.asBStr o))

procString :: TclProc
procString (f:s:args) 
 | f .== "trim" = treturn (T.trim s)
 | f .== "tolower" = retmod (B.map toLower) s
 | f .== "toupper" = retmod (B.map toUpper) s
 | f .== "length" = return $ T.mkTclInt (B.length `onObj` s)
 | f .== "index" = case args of 
                          [i] -> do ind <- T.asInt i
                                    if ind >= (B.length `onObj` s) || ind < 0 then ret else treturn $ B.singleton (B.index (T.asBStr s) ind)
                          _   -> tclErr $ "Bad args to string index."
 | otherwise            = tclErr $ "Can't do string action: " ++ show f
procString _ = tclErr $ "Bad string args"



procInfo [x] 
  | x .== "commands" =  getFrame >>= procList . toObs . Map.keys . procs
  | x .== "vars"     =  getFrame >>= procList . toObs . Map.keys . vars
  | otherwise        =  tclErr $ "Unknown info command: " ++ show (T.asBStr x)
procInfo _   = argErr "info"

toObs = map T.mkTclBStr

procAppend args = case args of
            (v:vx) -> do val <- varGet (T.asBStr v) `catchError` \_ -> ret
                         procSet [v, oconcat (val:vx)]
            _  -> argErr "append"
 where oconcat = T.mkTclBStr . B.concat . map T.asBStr

procList, procLindex, procLlength :: TclProc
procList a = treturn $ (map (escape . T.asBStr) a) `joinWith` ' '
 where escape s = if B.elem ' ' s then B.concat [B.singleton '{', s, B.singleton '}'] else s

procLindex args = case args of
          [l]   -> return l
          [l,i] -> do items <- T.asList l
                      ind <- T.asInt i
                      if ind >= length items then ret else treturn (items !! ind)
          _     -> argErr "lindex"

procLlength args = case args of
        [lst] -> if B.null `onObj` lst 
                        then return T.tclFalse
                        else liftM (T.mkTclInt . length) (T.asList lst) 
        _     -> argErr "llength"

procIncr :: TclProc
procIncr [vname]     = incr vname 1
procIncr [vname,val] = T.asInt val >>= incr vname
procIncr _           = argErr "incr"
incr n i =  do v <- varGet bsname
               intval <- T.asInt v
               let res = (T.mkTclInt (intval + i))
               varSet bsname res
               return res
 where bsname = T.asBStr n


procIf (cond:yes:rest) = do
  condVal <- doCond cond
  if condVal then evalTcl yes
    else case rest of
          [s,blk] -> if s .== "else" then evalTcl blk else tclErr "Invalid If"
          (s:r)   -> if s .== "elseif" then procIf r else tclErr "Invalid If"
          []      -> ret
procIf _ = argErr "if"

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
procError _   = argErr "error"

procUpLevel args = case args of
              [p]    -> uplevel 1 (procEval [p])
              (si:p) -> T.asInt si >>= \i -> uplevel i (procEval p)
              _      -> argErr "uplevel"

procUpVar args = case args of
     [d,s]    -> upvar 1 d s
     [si,d,s] -> T.asInt si >>= \i -> upvar i d s
     _        -> argErr "upvar"

procGlobal lst@(_:_) = mapM_ inner lst >> ret
 where inner g = do lst <- getStack
                    let len = length lst - 1
                    upvar len g g
procGlobal _         = argErr "global"


type ArgList = [Either BString (BString,BString)]

showParams (n,hasArgs,pl) = show ((n:(map arg2name pl)) `joinWith` ' ') ++ if hasArgs then " ..." else ""

arg2name arg = case arg of
               Left s      -> s
               Right (k,_) -> B.cons '?' (B.snoc k '?')

type ParamList = (BString, Bool, ArgList)

mkParamList :: BString -> ArgList -> ParamList
mkParamList name lst = (name, hasArgs, used)
 where hasArgs = (not . null) lst && (lastIsArgs lst)
       used = if hasArgs then init lst else lst
       lastIsArgs = either (.== "args") (const False)  . last

procProc [name,args,body] = do
  let pname = T.asBStr name
  params <- parseParams pname args
  pbody <- getParsed body
  regProc pname (procRunner params pbody)
procProc x = tclErr $ "proc: Wrong arg count (" ++ show (length x) ++ "): " ++ show (map T.asBStr x)

parseParams :: BString -> T.TclObj -> TclM ParamList
parseParams name args = T.asList args >>= countRet
 where countRet lst = mapM doArg lst >>= return . mkParamList name
       doArg s = do l <- T.asList s
                    return $ case l of
                              [k,v] -> Right (k,v)
                              _     -> Left s

procRunner :: ParamList -> [[TclWord]] -> [T.TclObj] -> TclM RetVal
procRunner pl body args = 
  withScope $ do 
    bindArgs pl args
    runCmds body `catchError` herr
 where herr (ERet s)  = return s
       herr (EDie s)  = tclErr s
       herr EBreak    = tclErr "invoked \"break\" outside of a loop"
       herr EContinue = tclErr "invoked \"continue\" outside of a loop"

bindArgs params@(_,hasArgs,pl) args = do
    walkBoth pl args 
  where walkBoth ((Left v):xs)      (a:as) = varSet v a >> walkBoth xs as
        walkBoth ((Left _):_)       []     = badArgs
        walkBoth ((Right (k,_)):xs) (a:as) = varSet k a >> walkBoth xs as
        walkBoth ((Right (k,v)):xs) []     = varSet k (T.mkTclBStr v) >> walkBoth xs []
        walkBoth []                 xl     = if hasArgs then do procList xl >>= varSet (B.pack "args")
                                                        else unless (null xl) badArgs
        badArgs = argErr $ "should be " ++ showParams params

joinWith bsl c = B.concat (intersperse (B.singleton c) bsl)
                   
treturn = return . T.mkTclBStr 

interp :: BString -> TclM RetVal
interp str = case wrapInterp str of
                   Left s  -> treturn s
                   Right x -> handle x
 where f (Left match)   = varGet match
       f (Right x) = runCommand x
       handle (b,m,a) = do mid <- f m
                           let front = B.append b (T.asBStr mid)
                           interp a >>= \v -> treturn (B.append front (T.asBStr v))

-- # TESTS # --

parseArrRef str = do start <- B.elemIndex '(' str
                     guard (start /= 0)
                     guard (B.last str == ')')
                     let (pre,post) = B.splitAt start str
                     return (pre, B.tail (B.init post))


run :: TclM RetVal -> Either Err RetVal -> IO Bool
run t v = do chans <- newIORef Map.empty
             ret <- liftM fst (runTclM t (TclState chans [baseEnv]))
             return (ret == v)

testProcEq = TestList [
      "1 eq 1 -> t" ~:          (procEq [int 1, int 1]) `is` True
      ,"1 == 1 -> t" ~:         (procEql [int 1, int 1]) `is` True
      ,"' 1 ' == 1 -> t" ~:     procEql [str " 1 ", int 1] `is` True
      ,"' 1 ' eq 1 -> f" ~:     procEq [str " 1 ", int 1] `is` False
      ,"' 1 ' eq ' 1 ' -> t" ~: procEq [str " 1 ", str " 1 "] `is` True
      ,"' 1 ' ne '1' -> t" ~: procNe [str " 1 ", str "1"] `is` True
      ,"'cats' eq 'cats' -> t" ~: procEq [str "cats", str "cats"] `is` True
      ,"'cats' eq 'caps' -> f" ~: procEq [str "cats", str "caps"] `is` False
      ,"'cats' ne 'cats' -> t" ~: procNe [str "cats", str "cats"] `is` False
      ,"'cats' ne 'caps' -> f" ~: procNe [str "cats", str "caps"] `is` True
   ]
 where (?=?) a b = assert (run b (Right a))
       is c b = (T.fromBool b) ?=? c
       int i = T.mkTclInt i
       str s = T.mkTclStr s

testArr = TestList [
   "december" `should_be` Nothing
   ,"dec(mber" `should_be` Nothing
   ,"dec)mber" `should_be` Nothing
   ,"(cujo)" `should_be` Nothing
   ,"de(c)mber" `should_be` Nothing
   ,"a(1)"          ?=> ("a","1")
   ,"xx(september)" ?=> ("xx","september")
   ,"arr(3,4,5)"    ?=> ("arr","3,4,5")
   ,"arr()"         ?=> ("arr","")
 ]
 where (?=>) a b@(b1,b2) = (a ++ " -> " ++ show b) ~: parseArrRef (B.pack a) ~=? Just (B.pack b1, B.pack b2)
       should_be x r =  (x ++ " should be " ++ show r) ~: parseArrRef (B.pack x) ~=? r
hiccupTests = TestList [ testProcEq, testArr ]

runUnit = runTestTT hiccupTests

-- # ENDTESTS # --
