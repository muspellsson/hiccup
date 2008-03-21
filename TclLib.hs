module TclLib (libCmds) where

import Common
import TclLib.IOProcs
import TclLib.ListProcs
import TclLib.ArrayProcs
import TclLib.ControlProcs
import TclLib.StringProcs
import TclLib.NSProcs
import TclLib.MathProcs (mathProcs)
import TclLib.UtilProcs

libCmds = mergeCmdMaps [ controlProcs, mathProcs, nsProcs, 
                        ioProcs, listProcs, arrayProcs, stringProcs, utilProcs ]
