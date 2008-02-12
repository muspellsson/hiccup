module ProcArgs (parseParams, bindArgs) where
import Util
import qualified TclObj as T
import Common
import qualified Data.ByteString.Char8 as B

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
       lastIsArgs = either (== (pack "args")) (const False)  . last


parseParams :: BString -> T.TclObj -> TclM ParamList
parseParams name args = T.asList (T.asBStr args) >>= countRet
 where countRet lst = mapM doArg lst >>= return . mkParamList name
       doArg s = do l <- T.asList s
                    return $ case l of
                              [k,v] -> Right (k,v)
                              _     -> Left s

bindArgs params@(_,hasArgs,pl) args = walkBoth pl args
  where walkBoth ((Left v):xs)      (a:as) = varSetLocalVal v a >> walkBoth xs as
        walkBoth ((Left _):_)       []     = badArgs
        walkBoth ((Right (k,_)):xs) (a:as) = varSetLocalVal k a >> walkBoth xs as
        walkBoth ((Right (k,v)):xs) []     = varSetLocalVal k (T.mkTclBStr v) >> walkBoth xs []
        walkBoth []                 xl     = if hasArgs then varSetLocalVal (pack "args") (T.mkTclList xl)
                                                        else if null xl then ret else badArgs
        badArgs = argErr $ "should be " ++ showParams params
