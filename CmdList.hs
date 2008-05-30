module CmdList where
import Types
import Util

data CmdSafety = CSafe | CUnsafe deriving (Eq,Show)

type CmdSpec = (String,TclCmd)
data CmdList = CmdList { unCmdList :: [CmdSpec] }

makeCmdList = makeNsCmdList ""
makeNsCmdList p = CmdList . mapFst (\n -> p ++ n)

mergeCmdLists :: [CmdList] -> CmdList
mergeCmdLists = CmdList . concat . map unCmdList
