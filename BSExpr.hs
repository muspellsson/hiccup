{-# LANGUAGE BangPatterns,OverloadedStrings #-}
module BSExpr where

import BSParse
import TclParse
import Util hiding (orElse)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Test.HUnit hiding (Node)

{-

 expr ::= item (op item)? 
 item ::= pre? atom


 -}

parseInt :: Parser Int 
parseInt s = case B.readInt s of
                Just x -> return x
                Nothing -> fail "expected int"

data Atom = AStr BString | ANum !Int | AVar BString
           | AFun BString Expr | ACom TokCmd deriving (Eq,Show)

data UnOp = OpNeg | OpNot deriving (Eq,Show)
data Expr = Item Atom | BinApp Expr Oper Expr | UnApp UnOp Expr | Paren Expr deriving (Eq,Show)


showExpr (Item (ANum i)) = show i
showExpr (Item x) = show x
showExpr (Paren e) = showExpr e
showExpr (UnApp o e) = "(" ++ show o ++ " " ++ showExpr e ++ ")"
showExpr (BinApp a op b) = showOpExpr (getOpName op) a b

showOpExpr ops a b = "(" ++ ops ++ " " ++ showExpr a ++ " " ++ showExpr b ++ ")"

parseShow s = case parseExpr (B.pack s) of
                Left r -> r
                Right (a,_) -> showExpr a

parseAtom = choose [str,var,cmd,num,fun]
 where  atom f w = (eatSpaces .>> f) `wrapWith` w
        str = atom parseStr AStr
        num = atom parseInt ANum
        var = atom doVarParse AVar
        cmd = atom parseSub ACom
        fun = atom (pjoin (,) (getPred1 wordChar "function name") (paren parseItem)) (\(x,y) -> AFun x y)

        
parseUnOp = notop `orElse` negop
  where negop = pchar '-' `wrapWith` (const OpNeg)
        notop = pchar '!' `wrapWith` (const OpNot)

parseItem = (parseAtom `wrapWith` Item) `orElse` (pjoin UnApp parseUnOp parseItem)

parseExpr = eatSpaces .>> expTerm
 where expTerm str = do
        (i1,r) <- (parseItem `orElse` paren parseExpr) str
        (pjoin (\a i2 -> fixApp i1 a i2) parseOp parseExpr) `orElse` (emit i1) $ r 


fixApp a@(BinApp a2 op2 b2) op b = if op `higherPrec` op2 then (BinApp a2 op2 (BinApp b2 op b)) else BinApp a op b
fixApp a op b@(BinApp a2 op2 b2) = if op `higherPrec` op2 then (BinApp (BinApp a op a2) op2 b2) else BinApp a op b
fixApp a op b = BinApp a op b

getPrec op = case op of
              OpEql -> 1
              OpPlus -> 2 
              OpMinus -> 2
              OpTimes -> 3
higherPrec op1 op2 = getPrec op1 >= getPrec op2

paren p = (pchar '(' .>> p) `pass` (eatSpaces .>> pchar ')')

data Oper = OpPlus | OpMinus | OpTimes | OpEql deriving (Eq,Show, Ord)

operators = [OpDef "+" OpPlus 2, OpDef "-" OpMinus 2, OpDef "*" OpTimes 3, OpDef "==" OpEql 1]

getOpName op = case M.lookup op opsByOper of
                 Just v -> B.unpack $ opName v
                 Nothing -> error "wtf"
opName (OpDef n _ _) = n
opsByOper = M.fromList (map pairer operators)
  where pairer a@(OpDef _ o _) = (o,a)

data OpDef = OpDef BString Oper Int

parseOp = (choose [op '*' OpTimes, op '+' OpPlus, op '-' OpMinus, sop "==" OpEql])
 where op c v = eatSpaces .>> pchar c .>> emit v
       sop s v = eatSpaces .>> parseLit s .>> emit v


bsExprTests = TestList [futureTests]

futureTests = "future" ~: TestList [atomTests, intTests, itemTests, exprTests] where
  num i = Item (ANum i)
  str s = Item (AStr s)
  should_be_ p dat res = (B.unpack dat) ~: Right (res, "") ~=? p dat
  atomTests = TestList [
     "11" `should_be` (ANum 11)
     ,"sin(4)" `should_be` (AFun "sin" (num 4))
     ,"$candy" `should_be` (AVar "candy")
     ,"\"what\"" `should_be` (AStr "what")
     ,"[incr x]" `should_be` (ACom (Word "incr", [Word "x"])) 
   ] where should_be = should_be_ parseAtom

  itemTests = TestList [
     "11" `should_be` (num 11)
     ,"!0" `should_be` (UnApp OpNot (num 0))
     ,"--4" `should_be` (UnApp OpNeg (Item (ANum (-4))))
     ,"![is_done]" `should_be` (UnApp OpNot (Item (ACom (Word "is_done", []))))
   ] where should_be = should_be_ parseItem
 
  exprTests = TestList [
     "11" `should_be` (Item (ANum 11))
     ,"(1 * 2) + 1" `should_be` (BinApp (BinApp  (num 1) OpTimes (num 2)) OpPlus (num 1))
     ,"1 * 2 + 1" `should_be` (BinApp (BinApp  (num 1) OpTimes (num 2)) OpPlus (num 1))
     ,"1 + 2 * 1" `should_be` (BinApp (num 1) OpPlus (BinApp (num 2) OpTimes (num 1)))
     ,"1 == \"1\"" `should_be` (BinApp (num 1) OpEql (str "1"))
   ] where should_be dat res = (B.unpack dat) ~: Right (res, "") ~=? parseExpr dat

  intTests = TestList [
       "44" `should_be` 44
       ,"9" `should_be` 9
       ,"catfish" `should_fail` ()
    ] where should_be dat res = Right (res,"") ~=? parseInt dat
            should_fail dat () = (parseInt dat) `should_fail_` ()

