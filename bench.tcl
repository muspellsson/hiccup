proc decr v {
  upvar $v loc
  incr loc -1
}

proc fib x { 
  if {<= $x 1} { 
    return 1
  } else { 
    return [+ [fib [ - $x 1 ]] [fib [ - $x 2 ]]]
  } 
}

proc fact x {
  set res 1
  while {<= 1 $x} {
    set res [* $x $res]
    decr x
  }
  return $res
}

proc memfib x {
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
  return $loc($x)
}

proc sum lst {
  set tot 0
  foreach i $lst {
    incr tot $i
  }
  return $tot
}

proc range {a b {step 1}} {
  set result {}
  for {set i $a} {<= $i $b} {incr i $step} {
    lappend result $i
  }
  return $result
}

set runcount 20

while {<= 1 $runcount } {
  for {set fcount 18} {< 2 $fcount} {decr fcount} {
    puts [fib $fcount]
    if { <= $fcount 2 } { break }
  }

  set fcount 25 
  while {<= 2 $fcount} {
    set x [memfib $fcount]
    decr fcount
  }

  set count 10
  while {<= 1 $count} {
    set x [fact $count]
    decr count
  }

  decr runcount
}

namespace eval Count {
  variable count 0

  proc bump { { step 1 } } {
    variable ::Count::count
    incr count $step
  }

  proc get {} {
    return $::Count::count
  }
}

proc repeat {n code} {
  for {set i 0} { <= $i $n } {incr i} $code
}

repeat 250 { Count::bump }

puts "Count: [Count::get]"


set top 3000
puts "Sum from 1 to ${top}: [sum [range 1 $top]]"
