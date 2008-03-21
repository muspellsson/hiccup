
proc approx_root { n { guess 5.0 } } {
  set iterations 10
  set prev $guess
  for { set i 10 } { > $i 0 } { incr i -1 } {
    set prev [expr { ($prev + ($n / $prev)) / 2 }]
  }
  return $prev
}

puts [approx_root 2]
puts [approx_root 64]

proc range {a b {step 1}} {
  set result {}
  for {set i $a} {<= $i $b} {incr i $step} {
    lappend result $i
  }
  return $result
}

set total 0
foreach i [range 2 500] {
  set total [expr { $total + [approx_root $i] }]
  puts "$i: [approx_root $i]"
}

