module Internal.InterpSpec where

import Util
import qualified TclObj as T
import CmdList
import Internal.Types
import TclChan (ChanMap, baseChans)


data InterpSpec = ISpec { ispecSafe :: Bool,
                          ispecVars :: [(BString,T.TclObj)],
                          ispecChans :: ChanMap,
                          ispecCmds :: CmdList,
                          ispecHidden :: CmdList,
                          ispecInits :: [TclM ()]
                        }


emptyInterp = ISpec { ispecSafe = True,
                      ispecVars = [],
                      ispecChans = baseChans,
                      ispecCmds = emptyCmdList,
                      ispecHidden = emptyCmdList,
                      ispecInits = [] }
