module RToken (Cmd(..), CmdName(..), RToken(..), singleTok, tryParsed, Parseable, Parsed, 
  tokCmdToCmd,
  TokResult,
  ExprResult,
  makeCExpr,
  asParsed, rtokenTests ) where

import qualified Data.ByteString.Char8 as B
import TclParse (TclWord(..), doInterp, runParse, TokCmd)
import Util (BString,pack)
import VarName
import Expr.Parse
import Expr.TExp (CExpr)
import Expr.Compile
import Test.HUnit

type Parsed = [Cmd]
type TokResult = Either String Parsed
type ExprResult = Either String (CExpr Cmd)
data CmdName = BasicCmd (NSQual BString) | DynCmd (RToken Cmd) deriving (Eq,Show)
data Cmd = Cmd CmdName [RToken Cmd] deriving (Eq,Show)
data RToken a = Lit !BString | LitInt !Int | CatLst [RToken a] 
              | CmdTok !a | ExpTok (RToken a)
              | VarRef !(NSQual VarName) | ArrRef !(Maybe NSTag) !BString (RToken a)
              | Block !BString TokResult ExprResult deriving (Eq,Show)


-- Bit hacky, but better than no literal handling
litIfy s 
 | B.length s == 1 = let c = B.index s 0 
                     in case c of
                          '0' -> LitInt 0
                          '1' -> LitInt 1
                          '2' -> LitInt 2
                          '3' -> LitInt 3
                          '4' -> LitInt 4
                          _   -> Lit s
 | otherwise       = Lit s


compile :: BString -> RToken Cmd
compile str = case doInterp str of
                   Left s  -> litIfy s
                   Right x -> handle x
 where f (Left match) = case parseVarName match of 
                          NSQual ns (VarName n (Just ind)) -> ArrRef ns n (compile ind)
                          vn                               -> VarRef vn
       f (Right x)    = compCmd x
       handle (b,m,a) = let front = [Lit b, f m]
                        in let lst = filter (not . isEmpty) (front ++ [compile a])
                           in case lst of 
                                [a] -> a
                                _   -> CatLst lst

isEmpty (Lit x)    = B.null x
isEmpty (CatLst l) = null l
isEmpty _          = False

makeCExpr = fromExpr . parseFullExpr

compToken :: TclWord -> (RToken Cmd)
compToken tw = case tw of
          Word s        -> compile s
          NoSub s res   -> Block s (fromParsed res) (makeCExpr s)
          Expand t      -> ExpTok (compToken t)
          Subcommand c  -> compCmd c

compCmd c = CmdTok (toCmd c)

class Parseable a where
  asParsed :: (Monad m) => a -> m Parsed

instance Parseable B.ByteString where
  asParsed s = case tryParsed s of
                  Left s -> fail s
                  Right p -> return p
  {-# INLINE asParsed #-}

tokCmdToCmd = toCmd 

tryParsed :: BString -> TokResult
tryParsed s = fromParsed (runParse s)
{-# INLINE tryParsed #-}

fromParsed m = case m of 
   Left w     -> Left $ "parse failed: " ++ w
   Right (r,rs) -> if B.null rs then Right (map toCmd r) else Left ("incomplete parse: " ++ show rs)

fromExpr m = case m of 
   Left w     -> Left $ "expr parse failed: " ++ w
   Right (r,rs) -> if B.null rs then Right (compileExpr toCmd r) else Left ("incomplete expr parse: " ++ show rs)

toCmd :: TokCmd -> Cmd
toCmd (x,xs) = Cmd (handleProc (compToken x)) (map compToken xs)
  where handleProc (Lit v) = BasicCmd (parseProc v)
        handleProc xx      = DynCmd xx

singleTok b = [toCmd (Word b,[])]

rtokenTests = TestList [compTests, compTokenTests] where
  compTests = "compile" ~: TestList [ 
      "x -> x" ~: "x" `compiles_to` (lit "x")  
      ,"$x -> VarRef x" ~: "$x" `compiles_to` (varref "x")  
      ,"x(G) -> ArrRef x G" ~: "$x(G)" `compiles_to` (arrref "x" (lit "G"))  
      ,"CatLst" ~: "$x$y" `compiles_to` (CatLst [varref "x", varref "y"])  
      ,"lit" ~: "incr x -1" `compiles_to` lit "incr x -1"
      ,"cmd" ~: "[double 4]" `compiles_to` cmdTok (BasicCmd (vlocal "double"), [ilit 4])
    ]

  compTokenTests = "compToken" ~: TestList [ 
      "1" ~: (mkwd "x")  `tok_to` (lit "x")  
      ,"2" ~: (mknosub "puts 4") `tok_to` (block "puts 4" [Cmd (BasicCmd (vlocal "puts")) [ilit 4]])
    ]
  
  block s v = Block (pack s) (Right v) ((fromExpr . parseFullExpr . pack) s)
  mknosub s = NoSub (pack s) (runParse (pack s))
  mkwd = Word . pack
  lit = Lit . pack 
  ilit = LitInt 
  vlocal x = NSQual Nothing (pack x)
  cmdTok (n,a) = CmdTok (Cmd n a)
  varref = VarRef . parseVarName . pack 
  arrref s t = ArrRef Nothing (pack s) t
  tok_to a b = do let r = compToken a
                  assertEqual (show a ++ " compiles to " ++ show b) b r
  compiles_to a b = do let r = compile (pack a)
                       assertEqual (show a ++ " compiles to " ++ show b) b r
