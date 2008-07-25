{-# LANGUAGE BangPatterns #-}
module Proc.Params (parseParams, bindArgs, ParamList, listParams) where
import Util
import qualified TclObj as T
import Internal.Types (TclM,ArgSpec, ArgList, ParamList(..))
import Control.Monad
import qualified Data.ByteString.Char8 as B

showParams :: ParamList -> String
showParams (ParamList (n,hasArgs,pl)) = 
   show $ unpack ((n:(map (arg2name True) pl)) `joinWith` ' ') 
             ++ if hasArgs then " ..." else ""

arg2name q arg = case arg of
    Left s      -> s
    Right (k,_) -> if q then B.cons '?' (B.snoc k '?') else k


listParams (ParamList (_,hasArgs,al)) = 
     map (arg2name False) al ++ if hasArgs then [pack "args"] else []

mkParamList :: BString -> ArgList -> ParamList
mkParamList name lst = ParamList (name, hasArgs, used)
 where hasArgs = (not . null) lst && (lastIsArgs lst)
       used = if hasArgs then init lst else lst
       lastIsArgs = either (== (pack "args")) (const False)  . last


parseParams :: BString -> T.TclObj -> TclM ParamList
parseParams name args = T.asList args >>= countRet
 where countRet = liftM (mkParamList name) . mapM parseArgSpec 
       parseArgSpec :: T.TclObj -> TclM ArgSpec
       parseArgSpec s = do 
                    l <- T.asList s
                    case l of
                       [k,v] -> return $ Right (T.asBStr k,v)
                       [_]   -> return $ Left (T.asBStr s)
                       _     -> fail $ "too many fields in argument specifier " ++ show (T.asBStr s)

bindArgs :: ParamList -> [T.TclObj] -> TclM [(BString,T.TclObj)]
bindArgs params@(ParamList (_,hasArgs,pl)) args = walkBoth pl args [] 
  where walkBoth ((Left v):xs)      (a:as) !acc = walkBoth xs as ((v,a):acc)
        walkBoth ((Left _):_)       []      _   = badArgs
        walkBoth ((Right (k,_)):xs) (a:as) !acc = walkBoth xs as ((k,a):acc)
        walkBoth ((Right (k,v)):xs) []     !acc = walkBoth xs [] ((k,v):acc)
        walkBoth []                 xl     !acc = if hasArgs then return $! ((pack "args"),(T.fromList xl)):acc
                                                             else if null xl then return $! acc 
                                                                             else badArgs
        badArgs = fail $ "should be " ++ showParams params
