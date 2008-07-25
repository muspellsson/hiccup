#!/bin/bash

#ruby WhenChanged.rb *.hs TclLib/*.hs atests.tcl atests/*.tcl Expr/*.hs Proc/*.hs testlib.tcl include.tcl
runhaskell fileWatcher.hs *.hs Internal/*.hs TclLib/*.hs atests.tcl atests/*.tcl Expr/*.hs Proc/*.hs testlib.tcl include.tcl
