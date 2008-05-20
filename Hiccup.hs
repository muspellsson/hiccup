{-# LANGUAGE BangPatterns #-}
module Hiccup (runTcl, runTclWithArgs, mkMainInterp, runInterp ) where

import Util
import Common
import TclLib.Interp
import qualified TclObj as T

import TclLib (libCmds)


baseCmds = mergeCmdLists [interpCmds, libCmds]
                          
processArgs al = [("argc" * T.fromInt (length al)), ("argv" * toTclList al)]
  where (*) name val = (pack name, val)
        toTclList = T.mkTclList . map T.fromBStr

interpVars = [("tcl_version" * (show hiccupVersion))]
  where (*) name val = (pack name, T.fromStr val)

hiccupVersion = 0.48

mkMainInterp = mkInterp baseCmds

runTcl v = mkMainInterp >>= runInterp v

runTclWithArgs v args = mkInterpWithVars mainVars baseCmds >>= runInterp v
 where mainVars = interpVars ++ (processArgs args)

