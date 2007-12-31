proc foreach {vname lst what} {
  set i 0
  while {< $i [llength $lst]} {
    uplevel "set $vname {[lindex $lst $i]}"
    uplevel $what
    incr i
  }
}

proc for {start test next body} {
  uplevel $start
  uplevel "while {$test} { eval {$body}; eval {$next} }"
}

proc decr v {
  upvar $v loc
  incr loc -1
}


