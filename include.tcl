
proc apply { fun args } {
  set len [llength $fun]
  if { [|| [< $len 2] [> $len 3]] } {
    error "can't interpret \"$fun\" as anonymous funtion"
  }

  lassign $fun argList body ns
}

proc decr v {
  upvar $v loc
  incr loc -1
}

proc range {a b {step 1}} {
  set res [list]
  for {set i $a} { <= $i $b } { incr i $step } {
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


