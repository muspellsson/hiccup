{-# LANGUAGE BangPatterns #-}
module Hiccup (runTcl, runTclWithArgs, mkMainInterp, interpEvalStr ) where

import Util
import TclLib.Interp
import TclLib.LibUtil
import qualified TclObj as T

import TclLib (libCmds)


baseCmds = mergeCmdLists [interpCmds, libCmds]
                          
processArgs al = [("argc" * T.fromInt (length al)), ("argv" * toTclList al)]
  where (*) name val = (pack name, val)
        toTclList = T.fromList . map T.fromBStr

interpVars al = [("tcl_version" * (show hiccupVersion))] ++ processArgs al
  where (*) name val = (pack name, T.fromStr val)

hiccupVersion = 0.49

mkMainInterp = mkInterp (interpVars []) baseCmds

runTcl v = mkMainInterp >>= interpEvalStr v

runTclWithArgs args v = mkInterp mainVars baseCmds >>= interpEvalStr v
 where mainVars = interpVars args

