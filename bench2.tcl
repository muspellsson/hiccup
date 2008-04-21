

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
    incr x -1
  }
  return $res
}

proc repeat { count code } {
    for {set i 1} { <= $i $count } { incr i } { uplevel $code }
}

proc run_things { {rep 1} {fci 3} { ci 11 } } {
  repeat $rep {
    for {set fcount $fci} {<= $fcount 22} {incr fcount} {
      puts [fib $fcount]
    }

    set count $ci
    while {<= 1 $count} {
      puts [fact $count]
      incr count -1
    }
  }
}

run_things 5 3 11
