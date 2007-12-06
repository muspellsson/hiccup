import Control.Monad.State
import qualified Data.Map as Map
import Control.Arrow
import System.IO
import Control.Monad.Error
import System.Exit
import Data.Char (toLower,toUpper)
import Data.List (intersperse)
import System.Environment
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import BSParse

type BString = B.ByteString

data Err = ERet !BString | EBreak | EContinue | EDie String deriving (Show,Eq)

instance Error Err where
 noMsg    = EDie "An error occurred."
 strMsg s = EDie s

data TclEnv = TclEnv { vars :: VarMap, procs :: ProcMap, upMap :: Map.Map BString (Int,BString) } 
type TclM = ErrorT Err (StateT [TclEnv] IO)
type TclProc = [BString] -> TclM RetVal
type ProcMap = Map.Map BString TclProc
type VarMap = Map.Map BString BString

type RetVal = BString -- IGNORE

procMap :: ProcMap
procMap = Map.fromList . map (B.pack *** id) $
 [("proc",procProc),("set",procSet),("upvar",procUpVar),("puts",procPuts),("gets",procGets),
  ("uplevel", procUpLevel),("if",procIf),("while",procWhile),("eval", procEval),("exit",procExit),
  ("list",procList),("lindex",procLindex),("llength",procLlength),("return",procReturn),
  ("break",procRetv EBreak),("catch",procCatch),("continue",procRetv EContinue),("eq",procEq),
  ("string", procString), ("append", procAppend), ("info", procInfo), ("global", procGlobal), ("source", procSource)]
   ++ map (id *** procMath) [("+",(+)), ("*",(*)), ("-",(-)), ("/",div), ("<", toI (<)),("<=",toI (<=)),("==",toI (==)),("!=",toI (/=))]

io :: IO a -> TclM a
io = liftIO

toI :: (Int -> Int -> Bool) -> (Int -> Int -> Int)
toI n a b = if n a b then 1 else 0

baseEnv = TclEnv { vars = Map.empty, procs = procMap, upMap = Map.empty }

main = do args <- getArgs 
          hSetBuffering stdout NoBuffering
          case args of
             [] -> run repl
             [f] -> B.readFile f >>= run . doTcl
 where run p = evalStateT (runErrorT (p `catchError` perr >> ret)) [baseEnv] >> return ()
       perr (EDie s) = io (hPutStrLn stderr s) >> ret
       display s = unless (B.null s) (io (B.putStrLn s)) 
       repl = do io (putStr "hiccup> ") 
                 eof <- io isEOF
                 if eof then ret
                  else do ln <- procGets []
                          if (not . B.null) ln then (doTcl ln `catchError` perr) >>= display >> repl else ret

ret = return B.empty

doTcl = runCmds . getParsed
runCmds = liftM last . mapM runCommand

getParsed str = case runParse str of 
                 Nothing -> error $ "parse error: " ++ (B.unpack str) 
                 Just (v,r) -> filter (not . null) v

(.==) bs str = bs == B.pack str
{-# INLINE (.==) #-}

doCond str = case getParsed str of
              [x]    -> runCommand x >>= return . isTrue
              _      -> tclErr "Too many statements in conditional"
 where isTrue = (/= B.singleton '0') . trim

trim = B.reverse . dropWhite . B.reverse . dropWhite

withScope :: TclM RetVal -> TclM RetVal
withScope f = do
  (o:old) <- get
  put $ (baseEnv { procs = procs o }) : o : old
  ret <- f
  get >>= put . tail
  return ret

set :: BString -> BString -> TclM ()
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
evalw (NoSub s)     = return s
evalw (Subcommand c) = runCommand c

ptrace = False -- IGNORE

runCommand :: [TclWord] -> TclM RetVal
runCommand [Subcommand s] = runCommand s
runCommand args = do 
 (name:evArgs) <- mapM evalw args
 --when ptrace $ io . print $ (name,args) -- IGNORE
 proc <- getProc name
 maybe (tclErr ("invalid command name: " ++ show name)) ($! evArgs) proc

procProc, procSet, procPuts, procIf, procWhile, procReturn, procUpLevel :: TclProc
procSet [s1,s2] = set s1 s2 >> return s2
procSet [s1] = varGet s1
procSet _    = tclErr $ "set: Wrong arg count"

procPuts s@(sh:st) = (io . mapM_ puts) txt >> ret
 where (puts,txt) = if sh .== "-nonewline" then (B.putStr,st) else (B.putStrLn,s)
procPuts x         = tclErr $ "Bad args to puts: " ++ show x

procGets [] = io B.getLine >>= return
procGets  _ = error "gets: Wrong arg count"

procEq [a,b] = return . B.singleton $! if a == b then '1' else '0'

procMath :: (Int -> Int -> Int) -> TclProc
procMath op [s1,s2] = liftM2 op (parseInt s1) (parseInt s2) >>= return . B.pack . show
procMath op _       = tclErr "math: Wrong arg count"

procEval [s] = doTcl s
procEval x   = tclErr $ "Bad eval args: " ++ show x

procSource [s] = io (B.readFile (B.unpack s)) >>= doTcl

procExit [] = io (exitWith ExitSuccess)

procCatch [s] = (doTcl s >> procReturn [B.pack "0"]) `catchError` (return . B.singleton . catchRes)
 where catchRes (EDie s) = '1'
       catchRes _        = '0'

procString (f:s:args) 
 | f .== "trim" = return (trim s)
 | f .== "tolower" = return (B.map toLower s)
 | f .== "toupper" = return (B.map toUpper s)
 | f .== "length" = return . B.pack . show . B.length $ s 
 | f .== "index" = case args of 
                          [i] -> do ind <- parseInt i
                                    if ind >= B.length s || ind < 0 then ret else return $ B.singleton (B.index s ind)
                          _   -> tclErr $ "Bad args to string index."
 | otherwise            = tclErr $ "Can't do string action: " ++ show f

tclErr = throwError . EDie

procInfo [x] = if x .== "commands" 
                 then get >>= procList . Map.keys . procs . head
                 else if x .== "vars" 
                        then get >>= procList . Map.keys . vars . head
                        else tclErr $ "Unknown info command: " ++ show x

procAppend (v:vx) = do val <- varGet v `catchError` \_ -> return B.empty
                       procSet [v,B.concat (val:vx)]

procList = return . B.concat . intersperse (B.singleton ' ') . map escape
 where escape s = if B.elem ' ' s then B.concat [B.singleton '{', s, B.singleton '}'] else s

procLindex [l,i] = do let items = map to_s . head . getParsed $ l
                      return . (!!) items =<< (parseInt i)

to_s (Word s)  = s
to_s (NoSub s) = s
to_s x         = error $ "to_s doesn't understand: " ++ show x

procLlength [lst] 
  | B.null lst = return (B.pack "0")
  | otherwise = return . B.pack . show . length . head . getParsed $ lst
procLlength x = tclErr $ "Bad args to llength: " ++ show x

procIf (cond:yes:rest) = do
  condVal <- doCond cond
  if condVal then doTcl yes
    else case rest of
          [s,blk] -> if s .== "else" then doTcl blk else error "Invalid If"
          (s:r)   -> if s .== "elseif" then procIf r else error "Invalid If"
          []      -> ret
procIf x = tclErr "Not enough arguments to If."

procWhile [cond,body] = loop `catchError` herr
 where pbody = getParsed body 
       herr EBreak    = ret
       herr (ERet s)  = return s
       herr EContinue = loop `catchError` herr
       herr x         = throwError x
       loop = do condVal <- doCond cond
                 if condVal then runCmds pbody >> loop else ret

procReturn [s] = throwError (ERet s)
procRetv c [] = throwError c
procError [s] = tclErr (B.unpack s)

parseInt si = maybe (tclErr ("Bad int: " ++ show si)) (return . fst) (B.readInt si)

procUpLevel [p]    = uplevel 1 (procEval [p]) 
procUpLevel (si:p) = parseInt si >>= \i -> uplevel i (procEval p)

uplevel i p = do 
  (curr,new) <- liftM (splitAt i) get 
  put new
  res <- p
  get >>= put . (curr ++)
  return res

procUpVar [d,s]    = upvar 1 d s
procUpVar [si,d,s] = parseInt si >>= \i -> upvar i d s

procGlobal lst@(_:_) = mapM_ inner lst >> ret
 where inner g = do lst <- get
                    let len = length lst - 1
                    upvar len g g

upvar n d s = do (e:es) <- get 
                 put ((e { upMap = Map.insert s (n,d) (upMap e) }):es)
                 ret

procProc [name,body]      = regProc name (procRunner [] (getParsed body))
procProc [name,args,body] = do
  let params = case parseArgs args of
                 Nothing -> error "Parse failed."
                 Just (r,_) -> map to_s r
  let pbody = getParsed body
  regProc name (procRunner params pbody)

procProc x = tclErr $ "proc: Wrong arg count (" ++ show (length x) ++ "): " ++ show x


procRunner :: [BString] -> [[TclWord]] -> [BString] -> TclM RetVal
procRunner pl body args = withScope $ do mapM_ (uncurry set) (zip pl args)
                                         when ((not . null) pl && (last pl .== "args")) $ do
                                               val <- procList (drop ((length pl) - 1) args) 
                                               set (B.pack "args") val 
                                         runCmds body `catchError` herr
 where herr (ERet s) = return s
       herr x        = error (show x)

varGet name = do env <- liftM head get
                 case upped name env of
                   Nothing -> maybe (tclErr ("can't read \"$" ++ B.unpack name ++ "\": no such variable")) 
                                    return 
                                    (Map.lookup name (vars env))
                   Just (i,n) -> uplevel i (varGet n) 
                   

interp :: BString -> TclM RetVal
interp str = case wrapInterp str of
                   Left s -> return s
                   Right x -> handle x
 where f (Word match) = varGet match
       f x            = runCommand [x]
       handle (b,m,a) = do mid <- f m
                           let front = B.append b mid
                           interp a >>= return . B.append front
