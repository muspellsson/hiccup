module TclLib (libCmds, libInits) where

import TclLib.LibUtil
import TclLib.IOCmds
import TclLib.ListCmds
import TclLib.ArrayCmds
import TclLib.ControlCmds
import TclLib.StringCmds
import TclLib.NSCmds
import TclLib.MathProcs (mathCmds)
import TclLib.UtilCmds
import TclLib.CoreCmds (coreCmds)

libInits = [setupEnv]
libCmds = mergeCmdLists [ controlCmds, mathCmds, coreCmds, nsCmds, 
                          ioCmds, listCmds, arrayCmds, stringCmds, utilCmds ]
