{-# LANGUAGE BangPatterns,OverloadedStrings #-}
module Expr.Eval ( runCExpr, Callback, CBData(..), exprEvalTests) where
import Expr.TExp
import Expr.Parse
import Expr.Compile
import qualified TclObj as T
import VarName
import Util
import RToken (Cmd, subCmdToCmds, RTokCmd)
import qualified Data.Map as M
import Expr.Util
import Test.HUnit

data CBData = VarRef (NSQual VarName) | FunRef (BString, [T.TclObj]) 
            | CmdEval [Cmd] | TokEval RTokCmd

type Callback m = (CBData -> m T.TclObj)


type CExprT = CExpr [Cmd] RTokCmd

runCExpr :: (Monad m) => Callback m -> CExprT -> m T.TclObj
runCExpr lu exp = run exp
 where run e = case e of
                 CItem v -> getItem v
                 CStrTok t -> lu (TokEval t)
                 DItem v -> getDep v
                 CTern a b c -> do
                   va <- run a >>= T.asBool
                   if va 
                       then run b
                       else run c
                 CApp2 f a b -> do
                            va <- run a
                            vb <- run b
                            (getOpFun f) va vb
                 CApp f a -> run a >>= (getUnFun f)
       callFun fn args = lu (FunRef (fn, args))
       getDep item = case item of
                        DVar vn   -> lu (VarRef vn)
                        DFun fn e -> mapM run e >>= callFun fn 
                        DCom cmd  -> lu (CmdEval cmd)
{-# INLINE runCExpr #-}

getItem item = case item of
                 ANum (TInt i)    -> return $! T.fromInt i
                 ANum (TDouble d) -> return $! T.fromDouble d
                 AStr s           -> return $! T.fromBStr s
                 ABlock s         -> return $! T.fromBStr s
{-# INLINE getItem #-}


runExpr :: (Monad m) => Callback m -> Expr -> m T.TclObj
runExpr lu exp = run exp
 where run e = case e of
                Item v        -> getItem v
                DepItem v     -> getDep v
                TernIf a b c -> do
                   va <- run a >>= T.asBool
                   if va 
                       then run b
                       else run c
                BinApp op a b -> do 
                        va <- run a
                        vb <- run b
                        (getOpFun op) va vb
                UnApp op v -> run v >>= getUnFun op
                Paren e    -> run e
       callFun fn args = lu (FunRef (fn, args))
       getDep item = case item of
                        DVar vn   -> lu (VarRef vn)
                        DFun fn e -> mapM run e >>= callFun fn
                        DCom cmd  -> lu (CmdEval (subCmdToCmds cmd))


exprEvalTests = TestList [evalTests, varEvalTests] where
    mint v = T.fromInt v
    evalTests = TestList
      [ 
        (tInt 3) `eql` (mint 3),
        ((tInt 5) + (tInt 5)) `eql` (mint 10),
        (((tInt 8) - (tInt 5)) + (tInt 5)) `eql` (mint 8),
        (((tInt 8) - (tInt 5)) .> (tInt 5)) `eql` tclFalse,
        "5 >= 5 -> true" ~: ((tInt 5) .>= (tInt 5)) `eql` tclTrue,
        "5 <= 5 -> true" ~: ((tInt 5) .<= (tInt 5)) `eql` tclTrue,
        ((tInt 6) .<= (tInt 5)) `eql` tclFalse,
        "8 - 5 < 5 -> true" ~: (((tInt 8) - (tInt 5)) .< (tInt 5)) `eql` tclTrue
      ]
     where eql a b = (runExpr (return . make) a) ~=? Just b
           make (FunRef _) = T.fromStr "PROC"
           make _          = T.fromBStr "ERROR"
           tclFalse = T.fromBool False
           tclTrue = T.fromBool True
    
    var v = DepItem (DVar (parseVarName v))
    varEvalTests = TestList [
        "$num -> 4" ~: (var "num") `eql` (mint 4),
        ((var "num") + (tInt 3)) `eql` (mint 7),
        ((tInt 4) + ((var "num") - (tInt 1))) `eql` (mint 7),
        "$boo == \"bean\" -> true" ~: ((var "boo") `eq` (tStr "bean")) `eql` (T.fromBool True)
      ]
     where eql a b = (runExpr lu a) ~=? Just b
           table = M.fromList . mapFst pack $ [("boo", T.fromStr "bean"), ("num", T.fromInt 4)]
           lu :: (Monad m) => Callback m
           lu (VarRef (NSQual _ (VarName v _)))  = M.lookup v table
           lu (FunRef _) = return $ T.fromStr "PROC"
           lu (TokEval _) = return $ T.fromStr "TOKSTR"
           lu (CmdEval _) = return $ T.fromStr "CMD"
