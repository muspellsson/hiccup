module RToken (Cmd, toCmd, RToken(..), noInterp, singleTok, rtokenTests ) where
import qualified Data.ByteString.Char8 as B
import BSParse (TclWord(..), doInterp, runParse)
import Util (BString,pack)
import VarName
import Test.HUnit

type Cmd = (Either (NSQual BString) RToken, [RToken])
data RToken = Lit !BString | LitInt !Int | CatLst [RToken] 
              | CmdTok Cmd | ExpTok RToken
              | VarRef (NSQual VarName) | ArrRef (Maybe NSTag) !BString RToken 
              | Block !BString (Either String [Cmd]) deriving (Eq,Show)

isEmpty (Lit x)    = B.null x
isEmpty (CatLst l) = null l
isEmpty _          = False

noInterp tok = case tok of
   (CmdTok _) -> False
   (VarRef _) -> False
   (ArrRef _ _ _) -> False
   (ExpTok t) -> noInterp t
   (CatLst l) -> all noInterp l
   _          -> True


-- Bit hacky, but better than no literal handling
litIfy s 
 | B.length s == 1 = let c = B.index s 0 
                     in case c of
                          '0' -> LitInt 0
                          '1' -> LitInt 1
                          '2' -> LitInt 2
                          _   -> Lit s
 | otherwise       = Lit s


compile :: BString -> RToken
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

compToken :: TclWord -> RToken
compToken (Word s)               = compile s
compToken (NoSub s res)          = Block s (fromParsed res)
compToken (Expand t)             = ExpTok (compToken t)
compToken (Subcommand c)         = compCmd c

compCmd c = CmdTok (toCmd c)

fromParsed Nothing       = Left "parse failed"
fromParsed (Just (tl,v)) = if B.null v then Right (map toCmd tl) else Left ("incomplete parse: " ++ show v)


toCmd (x,xs) = (handleProc (compToken x), map compToken xs)
  where handleProc (Lit v) = Left (parseProc v)
        handleProc xx      = Right xx

singleTok b = [toCmd (Word b,[])]

rtokenTests = TestList [compTests, compTokenTests] where
  compTests = TestList [ 
      "x -> x" ~: "x" `compiles_to` (lit "x")  
      ,"$x -> VarRef x" ~: "$x" `compiles_to` (varref "x")  
      ,"x(G) -> ArrRef x G" ~: "$x(G)" `compiles_to` (arrref "x" (lit "G"))  
      ,"CatLst" ~: "$x$y" `compiles_to` (CatLst [varref "x", varref "y"])  
      ,"lit" ~: "incr x -1" `compiles_to` lit "incr x -1"
      ,"cmd" ~: "[double 4]" `compiles_to` cmdTok (Left (vlocal (pack "double")), [lit "4"])
    ]

  compTokenTests = TestList [ 
      "1" ~: (mkwd "x")  `tok_to` (lit "x")  
      ,"2" ~: (mknosub "puts 4") `tok_to` (block "puts 4" [((Left (vlocal (pack "puts"))), [lit "4"])])
    ]
  
  block s v = Block (pack s) (Right v)
  mknosub s = NoSub (pack s) (runParse (pack s))
  mkwd = Word . pack
  lit = Lit . pack 
  vlocal x = NSQual Nothing x
  cmdTok = CmdTok
  varref = VarRef . parseVarName . pack 
  arrref s t = ArrRef Nothing (pack s) t
  tok_to a b = do let r = compToken a
                  assertEqual (show a ++ " compiles to " ++ show b) b r
  compiles_to a b = do let r = compile (pack a)
                       assertEqual (show a ++ " compiles to " ++ show b) b r
