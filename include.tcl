proc foreach {vname lst what} {
  set i 0
  while {< $i [llength $lst]} {
    uplevel "set $vname {[lindex $lst $i]}"
    uplevel "set vthing \$$vname"
    uplevel $what
    incr i
  }
}

proc incr v {
  upvar $v loc
  set loc [+ $loc 1]
}

proc decr v {
  upvar $v loc
  set loc [- $loc 1]
}
