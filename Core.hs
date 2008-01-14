module Core (evalTcl, doCond, regProc) where

import Common
import BSParse (TclWord(..), wrapInterp)
import qualified Data.Map as Map
import qualified TclObj as T
import Control.Monad
import qualified Data.ByteString.Char8 as B

evalTcl s = runCmds =<< getParsed s

runCmds = liftM last . mapM runCommand

getParsed :: T.TclObj -> TclM [[TclWord]]
getParsed = T.asParsed

interp :: BString -> TclM RetVal
interp str = case wrapInterp str of
                   Left s  -> treturn s
                   Right x -> handle x
 where f (Left match) = case parseArrRef match of 
                         Nothing      -> varGet2 match Nothing
                         Just (n,ind) -> do inner <- interp ind
                                            varGet2 n (Just (T.asBStr inner))
       f (Right x)    = runCommand x
       handle (b,m,a) = do mid <- f m
                           let front = B.append b (T.asBStr mid)
                           interp a >>= \v -> treturn (B.append front (T.asBStr v))

getProc str = getFrame >>= (`ifNothing` ("invalid command name " ++ show str)) . Map.lookup str . procs
regProc name pr = modStack (\(x:xs) -> (x { procs = Map.insert name pr (procs x) }):xs) >> ret

evalToken :: TclWord -> TclM RetVal
evalToken (Word s)               = interp s
evalToken (NoSub s res)          = return $ T.mkTclBStrP s res
evalToken (Subcommand _ c)       = runCommand c

runCommand :: [TclWord] -> TclM RetVal
runCommand args = do 
 (name:evArgs) <- mapM evalToken args
 proc <- getProc (T.asBStr name) 
 proc evArgs

doCond :: T.TclObj -> TclM Bool
doCond str = do 
      p <- T.asParsed str
      case p of
        [x]      -> runCommand x >>= return . T.asBool
        _        -> tclErr "Too many statements in conditional"
