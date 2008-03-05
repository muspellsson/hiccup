{-# LANGUAGE BangPatterns #-}
module ProcArgs (parseParams, bindArgs) where
import Util
import qualified TclObj as T
import Common
import qualified Data.ByteString.Char8 as B

type ArgList = [Either BString (BString,T.TclObj)]

showParams (n,hasArgs,pl) = show ((n:(map arg2name pl)) `joinWith` ' ') ++ if hasArgs then " ..." else ""

arg2name arg = case arg of
               Left s      -> s
               Right (k,_) -> B.cons '?' (B.snoc k '?')

type ParamList = (BString, Bool, ArgList)

mkParamList :: BString -> ArgList -> ParamList
mkParamList name lst = (name, hasArgs, used)
 where hasArgs = (not . null) lst && (lastIsArgs lst)
       used = if hasArgs then init lst else lst
       lastIsArgs = either (== (pack "args")) (const False)  . last


parseParams :: BString -> T.TclObj -> TclM ParamList
parseParams name args = T.asList (T.asBStr args) >>= countRet
 where countRet lst = mapM doArg lst >>= return . mkParamList name
       doArg s = do l <- T.asList s
                    return $ case l of
                              [k,v] -> Right (k,T.mkTclBStr v)
                              _     -> Left s

bindArgs params@(_,hasArgs,pl) args = walkBoth pl args [] 
  where walkBoth ((Left v):xs)      (a:as) !acc = walkBoth xs as ((v,a):acc)
        walkBoth ((Left _):_)       []      _   = badArgs
        walkBoth ((Right (k,_)):xs) (a:as) !acc = walkBoth xs as ((k,a):acc)
        walkBoth ((Right (k,v)):xs) []     !acc = walkBoth xs [] ((k,v):acc)
        walkBoth []                 xl     !acc = if hasArgs then return $! ((pack "args"),(T.mkTclList xl)):acc
                                                             else if null xl then return $! acc 
                                                                             else badArgs
        badArgs = argErr $ "should be " ++ showParams params
