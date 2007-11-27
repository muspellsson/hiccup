module ExprParse (exprAsBool,exprAsStr) where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr
import Test.HUnit

lexer :: P.TokenParser ()
lexer = P.makeTokenParser emptyDef

integer = P.natural lexer
symbol  = P.symbol lexer
stringLit = P.stringLiteral lexer

data Op = OpDiv | OpPlus | OpMinus | OpTimes | OpEql | OpNeql |
          OpLt | OpGt | OpLte | OpGte | OpStrNe | OpStrEq
  deriving (Show,Eq)

data TExp = TInt !Integer | TOp Op TExp TExp | TStr String deriving (Show,Eq)

asBool (TInt i) = return (i /= 0)
asBool (TStr s) = return  $ s `elem` ["1", "true", "yes", "on"]
asBool _ = fail "Can't convert to bool"

asStr (TInt i) = return (show i)
asStr (TStr s) = return s
asStr _ = fail "Can't convert to string"


--exprAsBool s = undefined
exprAsBool s = do v <- expr s
                  v2 <- runExpr v
                  asBool v2

exprAsStr s = do v <- expr s
                 v2 <- runExpr v
                 asStr v2
   

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
  fromInteger i = TInt i

(.<) = TOp OpLt
(.<=) = TOp OpLte
(.>) = TOp OpGt
(.>=) = TOp OpGte

eq = TOp OpStrEq
ne = TOp OpStrNe

getInt (TInt i) = return i
getInt _        = fail "type error"

intapply f x y = do i1 <- getInt =<< runExpr x  
                    i2 <- getInt =<< runExpr y 
                    return $ TInt (f i1 i2)

strapply f x y = do i1 <- asStr =<< runExpr x  
                    i2 <- asStr =<< runExpr y 
                    return $ TInt (f i1 i2)

toI v = \a b -> if v a b then 1 else 0 
runExpr (TOp OpPlus a b) = intapply (+) a b
runExpr (TOp OpTimes a b) = intapply (*) a b
runExpr (TOp OpMinus a b) = intapply (-) a b
runExpr (TOp OpDiv a b) = intapply div a b
runExpr (TOp OpEql a b) = intapply (toI (==)) a b
runExpr (TOp OpNeql a b) = intapply (toI (/=)) a b
runExpr (TOp OpLt a b) = intapply (toI (<)) a b
runExpr (TOp OpGt a b) = intapply (toI (>)) a b
runExpr (TOp OpLte a b) = intapply (toI (<=)) a b
runExpr (TOp OpGte a b) = intapply (toI (>=)) a b
runExpr (TOp OpStrEq a b) = strapply (toI (==)) a b
runExpr (TOp OpStrNe a b) = strapply (toI (/=)) a b
runExpr v = return v
 
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

factor = do char '('
            x <- pexpr
            char ')'
            return x
         <|> myint <|> mystr <|> factor <?> "term"
            
myint = do i <- (integer <?> "integer")
           return $ TInt i
mystr = do s <- stringLit
           return $ TStr s
        <?> "string"


--- Unit Testing ---

ptest a (p,s) = a ~=? (tparse p s)
 where tparse p s = case parse p "" s of
                      Left err -> Nothing
                      Right res -> Just res

a ?=? x = ptest (Just a) x
isBad x = ptest Nothing x

aNumberTests = TestList
     ["ANumber1" ~: (TInt 3) ?=? (myint, "3"),
      --"ANumber2" ~: (TInt (-1)) ?=? (myint, "-1"),
      "ANumber3" ~: isBad (myint, "Button")]

stringTests = TestList [
    "string1" ~: (TStr "boo") ?=? (mystr, "\"boo\"") 
    ,"string2" ~: (TStr "") ?=? (mystr, "\"\"") 
    ,"string3" ~: (TStr "44") ?=? (mystr, "\"44\"") 
 ]

exprTests = TestList 
    [ "expr1" ~: (TInt 3) ?=? (pexpr, "3")
     ,"expr2" ~: ((TInt 3) + (TInt 1)) ?=? (pexpr, "3+1")
     ,"expr2 in paren" ~: ((TInt 3) + (TInt 1)) ?=? (pexpr, "(3+1)")
     ,"expr2 in paren 2" ~: ((TInt 3) + (TInt 1)) ?=? (pexpr, "((3)+1)")
     ,"expr3" ~: ((TInt 2) + (TInt 5) * (TInt 5)) ?=? (pexpr, "2+5*5")
     ,"lt" ~: ((TInt 2) .< (TInt 5)) ?=? (pexpr, "2 < 5")
     ,"lte" ~: ((TInt 2) .<= (TInt 5)) ?=? (pexpr, "2 <= 5")
     ,"gt" ~: ((TInt 5) .> (TInt 5)) ?=? (pexpr, "5 > 5")
     ,"gte" ~: ((TInt 6) .>= (TInt 5)) ?=? (pexpr, "6 >= 5")
     ,"string eq" ~: ((TStr "X") `eq` (TStr "Y")) ?=? (pexpr, "\"X\" eq \"Y\"")
     ,"string ne" ~: ((TStr "X") `ne` (TStr "Y")) ?=? (pexpr, " \"X\" ne \"Y\"")
 ]

evalTests = TestList
    [ 
      (TInt 3) `eql` (TInt 3),
      ((TInt 5) + (TInt 5)) `eql` (TInt 10),
      (((TInt 8) - (TInt 5)) + (TInt 5)) `eql` (TInt 8),
      (((TInt 8) - (TInt 5)) .> (TInt 5)) `eql` (TInt 0),
      ((TInt 5) .>= (TInt 5)) `eql` (TInt 1),
      ((TInt 5) .<= (TInt 5)) `eql` (TInt 1),
      ((TInt 6) .<= (TInt 5)) `eql` (TInt 0),
      (((TInt 8) - (TInt 5)) .< (TInt 5)) `eql` (TInt 1)
    ]
 where eql a b = (runExpr a) ~=? Just b

parseTests = TestList [ aNumberTests, stringTests, exprTests, evalTests ]

runUnit = runTestTT parseTests
