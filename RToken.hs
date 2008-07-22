module RToken (Cmd(..), CmdName(..), RToken(..), singleTok, Parseable, Parsed, 
  subCmdToCmds,
  RTokCmd,
  ParseResult,
  makeParsed,
  asParsed, rtokenTests ) where

import qualified Data.ByteString.Char8 as B
import TclParse (TclWord(..), runParse, parseSubstAll, Subst(..), escapeChar, SubCmd)
import Util (BString,pack)
import VarName
import Expr.Parse
import Expr.TExp (CExpr)
import Expr.Compile
import Test.HUnit

type Parsed = [Cmd]
type TokResult = Either String Parsed
type ExprResult = Either String (CExpr [Cmd])
type ParseResult = (TokResult,ExprResult)

data Cmd = Cmd CmdName [RTokCmd] deriving (Eq,Show)
data CmdName = BasicCmd (NSQual BString) | DynCmd RTokCmd deriving (Eq,Show)
type RTokCmd = RToken [Cmd]
data RToken a = Lit !BString | LitInt !Int | CatLst [RToken a] 
              | CmdTok !a | ExpTok (RToken a)
              | VarRef !(NSQual VarName) | ArrRef !(Maybe NSTag) !BString (RToken a)
              | Block !BString !ParseResult deriving (Eq,Show)

compToken :: TclWord -> RTokCmd
compToken tw = case tw of
          Word s        -> compile s
          NoSub s       -> Block s (makeParsed s)
          Expand t      -> ExpTok (compToken t)
          Subcommand c  -> compCmds c

makeParsed s = (tryParsed s, makeCExpr s)
{-# INLINE makeParsed #-}

compCmds c = CmdTok (subCmdToCmds c)

compile :: BString -> RTokCmd
compile str = clean . tconcat . map toTok . elift . parseSubstAll $ str
 where clean [r] = r 
       clean rl  = CatLst rl
       tconcat (Lit a:Lit b:xs) = tconcat (Lit (B.append a b):xs)
       tconcat (x:xs) = x : tconcat xs
       tconcat [] = []
       toTok x = case x of
          SStr s -> litIfy s
          SCmd c -> compCmds c
          SEsc c -> litIfy . B.singleton . escapeChar $ c
          SVar v -> case parseVarName v of 
                      NSQual ns (VarName n (Just ind)) -> ArrRef ns n (compile ind)
                      vn                               -> VarRef vn

elift x = case x of
            Left e -> error e
            Right (v,_) -> v


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


class Parseable a where
  asParsed :: (Monad m) => a -> m Parsed

instance Parseable B.ByteString where
  asParsed s = case tryParsed s of
                  Left s -> fail s
                  Right p -> return p
  {-# INLINE asParsed #-}

subCmdToCmds :: SubCmd -> [Cmd]
subCmdToCmds = map toCmd 
toCmd (x,xs) = let (a:as) = map compToken (x:xs)
               in Cmd (handleProc a) as
  where handleProc (Lit v) = BasicCmd (parseProc v)
        handleProc xx      = DynCmd xx

tryParsed :: BString -> TokResult
tryParsed m = case runParse m of 
   Left w     -> Left $ "parse failed: " ++ w
   Right (r,rs) -> if B.null rs then Right (subCmdToCmds r) 
                                else Left ("incomplete parse: " ++ show rs)

makeCExpr m = case parseFullExpr m of 
   Left w     -> Left $ "expr parse failed: " ++ w
   Right (r,rs) -> if B.null rs then Right (compileExpr subCmdToCmds r) 
                                else Left ("incomplete expr parse: " ++ show rs)


singleTok b = subCmdToCmds [(Word b,[])]

rtokenTests = TestList [compTests, compTokenTests] where
  compTests = "compile" ~: TestList [ 
      "x -> x" ~: "x" `compiles_to` (lit "x")  
      ,"$x -> VarRef x" ~: "$x" `compiles_to` (varref "x")  
      ,"x(G) -> ArrRef x G" ~: "$x(G)" `compiles_to` (arrref "x" (lit "G"))  
      ,"CatLst" ~: "$x$y" `compiles_to` (CatLst [varref "x", varref "y"])  
      ,"lit" ~: "incr x -1" `compiles_to` lit "incr x -1"
      ,"esced" ~: "one \\n two \\n three" `compiles_to` (lit "one \n two \n three")
      ,"cmd" ~: "[double 4]" `compiles_to` cmdTok (BasicCmd (vlocal "double"), [ilit 4])
    ]

  compTokenTests = "compToken" ~: TestList [ 
      "1" ~: (mkwd "x")  `tok_to` (lit "x")  
      ,"2" ~: (mknosub "puts 4") `tok_to` (block "puts 4" [Cmd (BasicCmd (vlocal "puts")) [ilit 4]])
    ]
  
  block s v = let bs = pack s in Block (pack s) (Right v, makeCExpr bs)
  mknosub s = NoSub (pack s)
  mkwd = Word . pack
  lit = Lit . pack 
  ilit = LitInt 
  vlocal x = NSQual Nothing (pack x)
  cmdTok (n,a) = CmdTok [(Cmd n a)]
  varref = VarRef . parseVarName . pack 
  arrref s t = ArrRef Nothing (pack s) t
  tok_to a b = do let r = compToken a
                  assertEqual (show a ++ " compiles to " ++ show b) b r
  compiles_to a b = do let r = compile (pack a)
                       assertEqual (show a ++ " compiles to " ++ show b) b r
