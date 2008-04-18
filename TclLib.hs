module TclLib (libCmds) where

import Common
import TclLib.IOProcs
import TclLib.ListProcs
import TclLib.ArrayProcs
import TclLib.ControlProcs
import TclLib.StringProcs
import TclLib.NSProcs
import TclLib.MathProcs (mathCmds)
import TclLib.UtilProcs
import TclLib.CoreCmds (coreCmds)

libCmds = mergeCmdLists [ controlProcs, mathCmds, coreCmds, nsProcs, 
                          ioProcs, listProcs, arrayProcs, stringProcs, utilProcs ]
