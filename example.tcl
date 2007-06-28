# Here is an example of some stuff hiccup can do.
# I think it's neat.

proc incr v {
  upvar 1 loc $v
  set loc [+ $loc 1]
}

proc decr v {
  upvar loc $v
  set loc [- $loc 1]
}

proc memfib x {
  set loc {1 1}
  set ctr 2
  while { <= $ctr $x } {
    set v1 [lindex $loc [- $ctr 1]]
    set v2 [lindex $loc [- $ctr 2]]
    set sum [+ $v1 $v2]
    set loc "$loc $sum"
    incr ctr
  }
  return [lindex $loc $x]
}

set fcount 21
puts "First $fcount fibonacci numbers in descending order:"
while {<= 2 $fcount} {
  puts -nonewline "[memfib $fcount] "
  decr fcount
}
puts "\nDone."

