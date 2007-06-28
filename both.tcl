
proc fib x { 
  if {<= $x 1} { 
    return 1
  } else { 
    return [+ [fib [ - $x 1 ]] [fib [ - $x 2 ]]]
  } 
}

proc fact {x} {
  set res 1
  while {<= 1 $x} {
    set res [* $x $res]
    set x [- $x 1]
  }
  return $res
}

set fcount 2
while {<= $fcount 22} {
  puts [fib $fcount]
  set fcount [+ $fcount 1]
}


set count 9
while {<= 1 $count} {
  puts [fact $count]
  set count [- $count 1]
}
