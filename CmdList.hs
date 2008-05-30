module CmdList where
import Types

data CmdSpec = CmdSpec String TclCmd Bool
data CmdList = CmdList { unCmdList :: [CmdSpec] }

cmdSpecName (CmdSpec n _ _) = n
cmdSpecCmd (CmdSpec _ cmd _) = cmd

makeCmdList = makeNsCmdList ""

makeNsCmdList :: String -> [(String,TclCmd)] -> CmdList
makeNsCmdList p = CmdList . map (\(n,v) -> CmdSpec (p ++ n) v False)

mergeCmdLists :: [CmdList] -> CmdList
mergeCmdLists = CmdList . concat . map unCmdList
