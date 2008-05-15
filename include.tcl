proc is_hiccup {} {
  expr { [info nameofexecutable] eq "hiccup" }
}

if { ![is_hiccup] } {
  proc lmap {f lst} {
    set result [list]
    foreach i $lst {
      lappend result [apply $f $i]
    }
    return $result
  }
}

proc decr v {
  upvar $v loc
  incr loc -1
}

proc range {a b {step 1}} {
  set res [list]
  for {set i $a} { $i <= $b } { incr i $step } {
    lappend res $i
  }
  return $res
}

proc sum lst { 
  set res 0
  foreach i $lst {
    incr res $i
  }
  return $res
}


