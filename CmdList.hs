module CmdList where
import Types

data CmdSpec = CmdSpec { cmdSpecName :: String, 
                         cmdSpecCmd :: TclCmd, 
                         cmdSpecSafe :: Bool }
data CmdList = CmdList { unCmdList :: [CmdSpec] }

makeCmdList = makeNsCmdList ""

makeNsCmdList :: String -> [(String,TclCmd)] -> CmdList
makeNsCmdList p = CmdList . map (toCmdSpec p)

toCmdSpec p (n,v) = CmdSpec (p ++ n) v False

mergeCmdLists :: [CmdList] -> CmdList
mergeCmdLists = CmdList . concat . map unCmdList
