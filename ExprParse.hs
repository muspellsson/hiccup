import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr
import Test.HUnit

lexer :: P.TokenParser ()
lexer = P.makeTokenParser emptyDef

whiteSpace = P.whiteSpace lexer
integer = P.natural lexer
symbol  = P.symbol lexer
stringLit = P.stringLiteral lexer

data Op = OpDiv | OpPlus | OpMinus | OpTimes | 
          OpLt | OpGt | OpLte | OpGte | OpStrNe | OpStrEq
  deriving (Show,Eq)

data TExp = TInt Integer | TOp Op TExp TExp | TFloat Double | TStr String deriving (Show,Eq)

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

getDub (TInt i)   = return (fromIntegral i)
getDub (TFloat d) = return d
getDub _          = fail "type error"

intapply f x y = do i1 <- getInt x  
                    i2 <- getInt y 
                    return $ TInt (f i1 i2)

runExpr (TOp OpPlus a b) = intapply (+) a b
runExpr (TOp OpTimes a b) = intapply (*) a b
runExpr (TOp OpMinus a b) = intapply (-) a b
runExpr (TOp OpDiv a b) = intapply div a b
 
expr :: Parser TExp
expr   = buildExpressionParser table factor

table = [[op "*" (OpTimes) AssocLeft, op "/" (OpDiv)  AssocLeft]
        ,[op "+" (OpPlus) AssocLeft, op "-" (OpMinus) AssocLeft] 
        ,[tryop "<=" (OpLte) AssocLeft, tryop ">=" (OpGte) AssocLeft] 
        ,[op "<" OpLt AssocLeft, op ">" OpGt AssocLeft]
        ,[op "eq" OpStrEq AssocLeft, op "ne" OpStrNe AssocLeft]
     ]
   where
     op s f assoc = Infix (do{ symbol s; return (TOp f)}) assoc
     tryop s f assoc = Infix (do{ try(symbol s); return (TOp f)}) assoc

factor = do char '('
            x <- expr
            char ')'
            return x
         <|> myint <|> mystr <?> "term"
            
myint = do i <- integer
           return $ TInt i
        <?> "integer"
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
    [ "expr1" ~: (TInt 3) ?=? (expr, "3")
     ,"expr2" ~: ((TInt 3) + (TInt 1)) ?=? (expr, "3+1")
     ,"expr2 in paren" ~: ((TInt 3) + (TInt 1)) ?=? (expr, "(3+1)")
     ,"expr2 in paren 2" ~: ((TInt 3) + (TInt 1)) ?=? (expr, "((3)+1)")
     ,"expr3" ~: ((TInt 2) + (TInt 5) * (TInt 5)) ?=? (expr, "2+5*5")
     ,"lt" ~: ((TInt 2) .< (TInt 5)) ?=? (expr, "2 < 5")
     ,"lte" ~: ((TInt 2) .<= (TInt 5)) ?=? (expr, "2 <= 5")
     ,"gt" ~: ((TInt 5) .> (TInt 5)) ?=? (expr, "5 > 5")
     ,"gte" ~: ((TInt 6) .>= (TInt 5)) ?=? (expr, "6 >= 5")
     ,"string eq" ~: ((TStr "X") `eq` (TStr "Y")) ?=? (expr, "\"X\" eq \"Y\"")
     ,"string ne" ~: ((TStr "X") `ne` (TStr "Y")) ?=? (expr, "\"X\" ne \"Y\"")
 ]

parseTests = TestList [ aNumberTests, stringTests, exprTests ]

runUnit = runTestTT parseTests
