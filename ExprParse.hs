module ExprParse (exprParseTests, expr) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr

import qualified TclObj as T
import qualified Data.Map as M

import Util
import Expr.TExp
import Expr.Util

import Test.HUnit

lexer :: P.TokenParser ()
lexer = P.makeTokenParser emptyDef

intOrFloat = P.naturalOrFloat lexer
symbol  = P.symbol lexer
parens = P.parens lexer
schar c = char c >> P.whiteSpace lexer
identifier  = P.identifier lexer
stringLit = P.stringLiteral lexer

expr s = case parse pexpr "" s of
           Left err -> fail $ "Bad parse: (" ++ s ++ "): " ++ show err
           Right res -> return res

pexpr :: Parser TExp
pexpr = do 
    many space 
    res <- myexpr
    many space
    eof
    return res

myexpr = buildExpressionParser table factor

table = [[op1 '*' (OpTimes) AssocLeft, op1 '/' (OpDiv)  AssocLeft]
        ,[op1 '+' (OpPlus) AssocLeft, op1 '-' (OpMinus) AssocLeft] 
        ,[op "==" (OpEql) AssocLeft, op "!=" (OpNeql) AssocLeft] 
        ,[op "eq" OpStrEq AssocLeft, op "ne" OpStrNe AssocLeft]
        ,[tryop "<=" (OpLte) AssocLeft, tryop ">=" (OpGte) AssocLeft] 
        ,[op1 '<' OpLt AssocLeft, op1 '>' OpGt AssocLeft]
        ,[op "&&" OpAnd AssocLeft, op "||" OpOr AssocLeft]
        ,[prefix '!' TNot]
     ]
   where
     op s f assoc = Infix (do{ symbol s; return (TOp f)}) assoc
     op1 s f assoc = Infix (do{ schar s; return (TOp f)}) assoc
     tryop s f assoc = Infix (do{ try(symbol s); return (TOp f)}) assoc
     prefix c f = Prefix (do { schar c; return f})

factor = nested <|> numval
         <|>  boolval <|> mystr <|> myvar <|> myfun <?> "term"
 where nested = parens myexpr

neg = (char '-' >> return True)
     <|> (char '+' >> return False)
     <|> return False
            
numval = do n <- neg
            let adj v = if n then negate v else v
            iorf <- intOrFloat 
            return $ case iorf of
                      Left  i -> tInt (adj (fromIntegral i))
                      Right f -> tFloat (adj f)


mystr = do s <- stringLit
           return $ tStr s
        <?> "string"

boolval = do
  b <- ((s "true" <|> try (s "on")) >> return True) 
       <|> ((s "false" <|> s "off") >> return False)
  return . TVal $ T.fromBool b
 where s str = symbol str

myvar = do char '$' 
           s <- identifier
           return $ TVar s
        <?> "variable"

myfun = do s <- identifier
           inner <- parens (myexpr `sepBy` (char ','))
           return $ TFun s inner


--- Unit Testing ---

ptest a (p,s) = a ~=? (tparse p s)
 where tparse p s = case parse p "" s of
                      Left _    -> Nothing
                      Right res -> Just res

a ?=? x = ptest (Just a) x
isBad x = ptest Nothing x

aNumberTests = TestList
     ["ANumber1" ~: (tInt 3) ?=? (numval, "3"),
      "double" ~: (tFloat 1.25) ?=? (numval, "1.25"),
      "ANumber2" ~: (tInt (-1)) ?=? (numval, "-1"),
      "ANumber3" ~: isBad (numval, "Button")]

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
     ,"fun1" ~: (TFun "sin" [tInt 44]) ?=? (pexpr, "sin(44)")
     ,"fun2" ~: ((tInt 11) + (TFun "sin" [tInt 44])) ?=? (pexpr, "11 + sin(44)")
     ,"fun3" ~: (TFun "rand" []) ?=? (pexpr, "rand()")
     ,"and expr" ~: ((tInt 1) .&& (tInt 2)) ?=? (pexpr, "1 && 2")
     ,"or expr" ~: (((tInt 1) + (tInt 1)) .|| (tInt 2)) ?=? (pexpr, "(1+1) || 2")
     ,"sin +" ~: ((TFun "sin" [tInt 0]) + (tInt 10)) ?=? (pexpr, "sin(0) + 10")
 ]

exprParseTests = TestList [ aNumberTests, stringTests, varTests, exprTests ]
