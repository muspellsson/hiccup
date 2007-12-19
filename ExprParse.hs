module ExprParse (exprAsBool,exprAsStr) where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr
import Test.HUnit

import qualified TclObj as T
import qualified Data.Map as M

lexer :: P.TokenParser ()
lexer = P.makeTokenParser emptyDef

integer = P.natural lexer
symbol  = P.symbol lexer
identifier  = P.identifier lexer
stringLit = P.stringLiteral lexer

data Op = OpDiv | OpPlus | OpMinus | OpTimes | OpEql | OpNeql |
          OpLt | OpGt | OpLte | OpGte | OpStrNe | OpStrEq
  deriving (Show,Eq)

data TExp = TOp Op TExp TExp | TVar String | TFun String TExp | TVal T.TclObj deriving (Show,Eq)

asBool lu v = 
 case v of
   TVal v -> return (T.asBool v)
   TVar v -> asBool lu (tStr v)
   _      -> fail "cannot convert to bool"

asStr lu v = case v of
         (TVal v) -> return (T.asStr v)
         (TVar s) -> lu s
         _        -> fail "Can't convert to string"

testLu :: String -> IO T.TclObj
testLu v = M.lookup v table
 where table = M.fromList [("boo", T.mkTclStr "bean"), ("num", T.mkTclInt 4)]

exprCompile :: (Monad m) => String -> m ((String -> m T.TclObj) -> m T.TclObj)
exprCompile s = do v <- expr s
                   return $ runExpr2 v
exprAsBool lu s = do v <- expr s
                     v2 <- runExpr lu v
                     asBool lu v2

exprAsStr lu s = do v <- expr s
                    v2 <- runExpr lu v
                    asStr lu v2
   

expr s = case parse pexpr "" s of
           Left err -> fail $ "Bad parse: (" ++ s ++ "): " ++ show err
           Right res -> return res

instance Num TExp where
  a + b = TOp OpPlus a b
  (-) = TOp OpMinus
  (*) = TOp OpTimes
  abs = undefined
  signum = undefined
  negate = undefined
  fromInteger i =  TVal (T.mkTclInt (fromIntegral i))

(.<) = TOp OpLt
(.<=) = TOp OpLte
(.>) = TOp OpGt
(.>=) = TOp OpGte
(.==) = TOp OpEql

eq = TOp OpStrEq
ne = TOp OpStrNe

getInt :: (Monad m) => (String -> m String) -> TExp -> m Int
getInt lu x = 
 case x of
    TVal i -> T.asInt i
    TVar s -> do val <- lu s
                 return (read val)
    _      -> fail "type error"

intapply :: (Monad m) => (String -> m String) -> (Int -> Int -> Int) -> TExp -> TExp -> m TExp
intapply lu f x y = do
  i1 <- getInt lu =<< runExpr lu x  
  i2 <- getInt lu =<< runExpr lu y 
  return $ tInt (f i1 i2)

objapply :: (Monad m) => (String -> m T.TclObj) -> ([T.TclObj] -> m T.TclObj) -> TExp -> TExp -> m T.TclObj
objapply lu f x y = do
  i1 <- runExpr2 x lu 
  i2 <- runExpr2 y lu
  f [i1,i2]

strapply lu f x y = do 
 i1 <- asStr lu =<< runExpr lu x  
 i2 <- asStr lu =<< runExpr lu y 
 return $ tInt (f i1 i2)

toI v = \a b -> if v a b then 1 else 0 

runExpr2 :: (Monad m) => TExp -> (String -> m T.TclObj) -> m T.TclObj
runExpr2 exp lu = 
  case exp of
    (TOp OpPlus a b) -> objap (procMath (+)) a b
    (TOp OpTimes a b) -> objap (procMath (*)) a b
    (TOp OpMinus a b) -> objap (procMath (-)) a b
    (TOp OpDiv a b) -> objap nop a b
    (TOp OpEql a b) -> objap nop a b
    (TOp OpNeql a b) -> objap nop a b
    (TOp OpLt a b) -> objap nop a b
    (TOp OpGt a b) -> objap nop a b
    (TOp OpLte a b) -> objap nop a b
    (TOp OpGte a b) -> objap nop a b
    (TOp OpStrEq a b) -> objap nop a b
    (TOp OpStrNe a b) -> objap nop a b
    (TVal v) -> return v
    (TVar n) -> lu n
    _                 -> fail $ "wtf?" ++ (show exp)
 where nop lst = fail "not implemented"
       objap = objapply lu
       procMath f [a,b] = do ai <- T.asInt a
                             bi <- T.asInt b
                             return $ T.mkTclInt (ai `f` bi)

runExpr :: (Monad m) => (String -> m String) -> TExp -> m TExp
runExpr lu expr =
  case expr of
    (TOp OpPlus a b) -> intap (+) a b
    (TOp OpTimes a b) -> intap (*) a b
    (TOp OpMinus a b) -> intap (-) a b
    (TOp OpDiv a b) -> intap div a b
    (TOp OpEql a b) -> intap (toI (==)) a b
    (TOp OpNeql a b) -> intap (toI (/=)) a b
    (TOp OpLt a b) -> intap (toI (<)) a b
    (TOp OpGt a b) -> intap (toI (>)) a b
    (TOp OpLte a b) -> intap (toI (<=)) a b
    (TOp OpGte a b) -> intap (toI (>=)) a b
    (TOp OpStrEq a b) -> strap (toI (==)) a b
    (TOp OpStrNe a b) -> strap (toI (/=)) a b
    _                 -> return expr
 where intap = intapply lu
       strap = strapply lu
 
pexpr :: Parser TExp
pexpr   = many space >> buildExpressionParser table factor

table = [[op "*" (OpTimes) AssocLeft, op "/" (OpDiv)  AssocLeft]
        ,[op "+" (OpPlus) AssocLeft, op "-" (OpMinus) AssocLeft] 
        ,[op "==" (OpEql) AssocLeft, op "!=" (OpNeql) AssocLeft] 
        ,[tryop "eq" OpStrEq AssocLeft, tryop "ne" OpStrNe AssocLeft]
        ,[tryop "<=" (OpLte) AssocLeft, tryop ">=" (OpGte) AssocLeft] 
        ,[op "<" OpLt AssocLeft, op ">" OpGt AssocLeft]
     ]
   where
     op s f assoc = Infix (do{ symbol s; return (TOp f)}) assoc
     tryop s f assoc = Infix (do{ try(symbol s); return (TOp f)}) assoc

factor = do symbol "("
            x <- pexpr
            symbol ")"
            return x
         <|> myint <|> mystr <|> myvar <|> myfun <|> factor <?> "term"
            
myint = do i <- (integer <?> "integer")
           return $ tInt (fromIntegral i)
mystr = do s <- stringLit
           return $ tStr s
        <?> "string"

myvar = do char '$' 
           s <- identifier
           return $ TVar s
        <?> "variable"

myfun = do s <- identifier
           char '('
           inner <- factor
           char ')'
           return $ TFun s inner


--- Unit Testing ---

ptest a (p,s) = a ~=? (tparse p s)
 where tparse p s = case parse p "" s of
                      Left err -> Nothing
                      Right res -> Just res

a ?=? x = ptest (Just a) x
isBad x = ptest Nothing x

aNumberTests = TestList
     ["ANumber1" ~: (tInt 3) ?=? (myint, "3"),
      --"ANumber2" ~: (tInt (-1)) ?=? (myint, "-1"),
      "ANumber3" ~: isBad (myint, "Button")]

stringTests = TestList [
    "string1" ~: (tStr "boo") ?=? (mystr, "\"boo\"") 
    ,"string2" ~: (tStr "") ?=? (mystr, "\"\"") 
    ,"string3" ~: (tStr "44") ?=? (mystr, "\"44\"") 
 ]

varTests = TestList [
    "var1" ~: (TVar "boo") ??= "$boo" 
    ,"var2" ~: (TVar "keebler") ??= "$keebler" 
    ,"var3" ~: bad "$" 
  ]
 where bad v = ptest Nothing (myvar,v)
       a ??= v = ptest (Just a) (myvar,v)
 
tInt i = TVal (T.mkTclInt i)
tStr s = TVal (T.mkTclStr s)

exprTests = TestList 
    [ "expr1" ~: (tInt 3) ?=? (pexpr, "3")
     ,"expr2" ~: ((tInt 3) + (tInt 1)) ?=? (pexpr, "3+1")
     ,"var+var" ~: ((TVar "a") + (TVar "b")) ?=? (pexpr, "$a+$b")
     ,"expr2 in paren" ~: ((tInt 3) + (tInt 1)) ?=? (pexpr, "(3+1)")
     ,"expr2 in paren 2" ~: ((tInt 3) + (tInt 1)) ?=? (pexpr, "((3)+1)")
     ,"expr3" ~: ((tInt 2) + (tInt 5) * (tInt 5)) ?=? (pexpr, "2+5*5")
     ,"lt" ~: ((tInt 2) .< (tInt 5)) ?=? (pexpr, "2 < 5")
     ,"lte" ~: ((tInt 2) .<= (tInt 5)) ?=? (pexpr, "2 <= 5")
     ,"gt" ~: ((tInt 5) .> (tInt 5)) ?=? (pexpr, "5 > 5")
     ,"gte" ~: ((tInt 6) .>= (tInt 5)) ?=? (pexpr, "6 >= 5")
     ,"string eq" ~: ((tStr "X") `eq` (tStr "Y")) ?=? (pexpr, "\"X\" eq \"Y\"")
     ,"string eq var" ~: ((TVar "X") `eq` (tStr "Y")) ?=? (pexpr, "$X eq \"Y\"")
     ,"string ne" ~: ((tStr "X") `ne` (tStr "Y")) ?=? (pexpr, " \"X\" ne \"Y\"")
     ,"varstorm1" ~: (((TVar "me") + (TVar "hey")) .< (TVar "something")) ?=? (pexpr, "($me + $hey) < $something")
     ,"varstorm2" ~: (((TVar "me") + (TVar "hey")) .< (TVar "something")) ?=? (pexpr, "($me + $hey)< $something")
     ,"varstorm3" ~: (((TVar "me") + (TVar "hey")) .< (TVar "st")) ?=? (pexpr, " ( $me+$hey ) <$st ")
     ,"varstorm4" ~: (((TVar "me") * (TVar "hey")) .== (TVar "st")) ?=? (pexpr, " $me * $hey  == $st ")
     ,"fun1" ~: (TFun "sin" (tInt 44)) ?=? (pexpr, "sin(44)")
     ,"fun2" ~: ((tInt 11) + (TFun "sin" (tInt 44))) ?=? (pexpr, "11 + sin(44)")
 ]

evalTests = TestList
    [ 
      (tInt 3) `eql` (tInt 3),
      ((tInt 5) + (tInt 5)) `eql` (tInt 10),
      (((tInt 8) - (tInt 5)) + (tInt 5)) `eql` (tInt 8),
      (((tInt 8) - (tInt 5)) .> (tInt 5)) `eql` (tInt 0),
      ((tInt 5) .>= (tInt 5)) `eql` (tInt 1),
      ((tInt 5) .<= (tInt 5)) `eql` (tInt 1),
      ((tInt 6) .<= (tInt 5)) `eql` (tInt 0),
      (((tInt 8) - (tInt 5)) .< (tInt 5)) `eql` (tInt 1)
    ]
 where eql a b = (runExpr return a) ~=? Just b

varEvalTests = TestList
    [ 
      ((TVar "num") + (tInt 3)) `eql` (tInt 7),
      ((tInt 4) + ((TVar "num") - (tInt 1))) `eql` (tInt 7),
      ((TVar "boo") `eq` (tStr "bean")) `eql` (tInt 1)
    ]
 where eql a b = (runExpr lu a) ~=? Just b
       table = M.fromList [("boo", "bean"), ("num", "4")]
       lu v = M.lookup v table

parseTests = TestList [ aNumberTests, stringTests, varTests, exprTests, evalTests, varEvalTests ]

runUnit = runTestTT parseTests

howbout s = parse pexpr "" s
