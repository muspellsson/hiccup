
namespace import ::tcl::mathop::*

proc fib x { 
  if {$x <= 1} { 
    return 1
  } else { 
    return [+ [fib [ - $x 1 ]] [fib [ - $x 2 ]]]
  }
}

proc fact {x} {
  set res 1
  while {1 <= $x} {
    set res [* $x $res]
    set x [- $x 1]
  }
  return $res
}

set fcount 3
while {$fcount <= 22} {
  puts [fib $fcount]
  set fcount [+ $fcount 1]
}


set count 11
while {1 <= $count} {
  puts [fact $count]
  set count [- $count 1]
}
