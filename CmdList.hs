module CmdList where
import Internal.Types

data CmdSpec = CmdSpec { cmdSpecName :: String, 
                         cmdSpecCmd :: TclCmd, 
                         cmdSpecSafe :: Bool }

data CmdList = CmdList { unCmdList :: [CmdSpec] }


onlySafe (CmdList cl) = CmdList (filter cmdSpecSafe cl)

makeCmdList = makeNsCmdList ""

makeNsCmdList :: String -> [(String,TclCmd)] -> CmdList
makeNsCmdList p = CmdList . map (toCmdSpec p)

safeCmds = map (\(n,c) -> (n,c,True))
unsafeCmds = map (\(n,c) -> (n,c,False))

nsCmdList p = CmdList . map (toCmdSpec_ p)

toCmdSpec p (n,v) = CmdSpec (p ++ n) v True

toCmdSpec_ p (n,v,s) = CmdSpec (p ++ n) v s

mergeCmdLists :: [CmdList] -> CmdList
mergeCmdLists = CmdList . concat . map unCmdList
