# Here is an example of some stuff hiccup can do.
# I think it's neat.

proc decr { v { i -1 } } {
  upvar $v loc
  incr loc $i
}

proc memfib x {
  set loc {1 1}
  set ctr 2
  while { <= $ctr $x } {
    set v1 [lindex $loc [- $ctr 1]]
    set v2 [lindex $loc [- $ctr 2]]
    set {the sum} [+ $v1 $v2]
    set loc "$loc ${the sum}"
    incr ctr
  }
  return [lindex $loc $x]
}

proc memfib2 x {
  set loc(0) 1
  set loc(1) 1
  set ctr 2
  while { <= $ctr $x } {
    set v1 "$loc([- $ctr 1])"
    set v2 "$loc([- $ctr 2])"
    set {the sum} [+ $v1 $v2]
    set loc($ctr) ${the sum}
    incr ctr
  }
  return "$loc($x)"
}

set fcount 21
puts "First $fcount fibonacci numbers in descending order:"
while {<= 2 $fcount} {
  puts -nonewline "[memfib $fcount] "
  puts -nonewline "[memfib2 $fcount] "
  decr fcount
}
puts "\nDone."

proc foreach {vname lst what} {
  set i 0
  while {< $i [llength $lst]} {
    uplevel "set $vname {[lindex $lst $i]}"
    uplevel $what
    incr i
  }
}

proc say_i_havent_been_to { args } {
  foreach name $args {
    puts "I've never been to $name."
  }
}

say_i_havent_been_to Spain China Russia Argentina "North Dakota"

