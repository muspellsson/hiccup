module TclLib (libCmds) where

import TclLib.LibUtil
import TclLib.IOCmds
import TclLib.ListProcs
import TclLib.ArrayProcs
import TclLib.ControlProcs
import TclLib.StringProcs
import TclLib.NSProcs
import TclLib.MathProcs (mathCmds)
import TclLib.UtilProcs
import TclLib.CoreCmds (coreCmds)

libCmds = mergeCmdLists [ controlCmds, mathCmds, coreCmds, nsCmds, 
                          ioCmds, listCmds, arrayProcs, stringCmds, utilProcs ]
