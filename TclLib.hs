module TclLib (libCmds, libInits) where

import TclLib.LibUtil
import TclLib.IOCmds (ioCmds, ioInits)
import TclLib.ListCmds
import TclLib.ArrayCmds
import TclLib.ControlCmds
import TclLib.StringCmds
import TclLib.NSCmds
import TclLib.MathProcs (mathCmds)
import TclLib.UtilCmds
import TclLib.CoreCmds (coreCmds, coreInits)

libInits = concat [coreInits, ioInits, stringInits, arrayInits]
libCmds = mergeCmdLists [ controlCmds, mathCmds, coreCmds, nsCmds, 
                          ioCmds, listCmds, arrayCmds, stringCmds, utilCmds ]
