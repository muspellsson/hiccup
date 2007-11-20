set assertcount 0

proc die s {
  puts $s
  exit
}

proc assertEq {a b} {
  if {== $a $b} {
    puts -nonewline "."
    uplevel {set assertcount [+ 1 $assertcount]}
  } else {
    die "Failed! $a != $b"
  }
}

proc assertStrEq {a b} {
  if {eq $a $b} {
    puts -nonewline "."
    uplevel {set assertcount [+ 1 $assertcount]}
  } else {
    die "Failed! \"$a\" != \"$b\""
  }
}

proc announce { 
  puts "Running tests"
}

announce

assertEq [eval {[* 4 4]}] 16


proc uptest {var v} {
  upvar loc $var
  set loc $v
}

proc incr v {
  upvar loc $v
  set loc [+ $loc 1]
}

set x 4
uptest x 3
assertEq $x 3

proc uptest2 {var2 v} {
  proc inner {a b} {
    upvar 2 whee $b 
    set whee $a
  }
  upvar lark $var2
  inner $v $var2 
  incr lark
}

set y 99 
uptest2 y 3
assertEq $y 4

if {== 3 4} {
 "This should be no problem. $woo_etcetera.; 
 "
} else {
 assertEq 1 1
}

proc decr v {
  upvar loc $v
  set loc [- $loc 1]
}

set count 0

incr count
incr count
incr count

assertEq $count 3

decr count
decr count
decr count

assertEq $count 0


set bean [list 1 2 3 4 5 {6 7 8}]

assertEq [llength $bean] 6
assertEq [lindex $bean 3] 4
assertEq [lindex $bean 5] {6 7 8}


if { eq "one" "two" } {
  die "Should not have hit this."
} elseif { == 1 1 } {
  assertEq 44 44
} else {
  die "Should not have hit this."
}

set total 0
proc argstest {tot args} {
  upvar total $tot
  set i 0
  while {< $i [llength $args]} {
    set total [+ [lindex $args $i] $total]
    incr i
  }
}

assertEq 0 $total
argstest total 1 2 3 4 5 6 7 8
assertEq 36 $total

set sval 0
set sval2 1
while {<= $sval 10} {
  incr sval
  if {<= 8 $sval} {
    break
  }
  if {<= 4 $sval} {
    continue
  }
  incr sval2
}

assertEq 8 $sval
assertEq 4 $sval2


assertEq 10 [+ 15 -5] # Check that negatives parse.

assertEq 4 [string length "five"]
assertEq 0 [string length ""]
assertEq 7 [string length "one\ntwo"]
assertEq 4 [string length "h\n\ti"]

set fst [string index "whee" 1]
assertStrEq "h" $fst


set somestr "one"
append somestr " two" " three" " four"
assertStrEq "one two three four" $somestr
append avar a b c
assertStrEq "abc" $avar


proc foreach {vname lst what} {
  set i 0
  while {< $i [llength $lst]} {
    uplevel "set $vname [lindex $lst $i]"
    uplevel "set vthing \$$vname"
    uplevel $what
    incr i
  }
}

set numbers {1 2 3 4 5}
set result 0
set vthing "bean"
foreach number $numbers {
  set result [+ $number $result]
}

assertEq 15 $result
assertEq 5 $vthing

set misc { 1 2 3 4 5 6 }
proc join { lsx mid } {
  set res ""
  set first_time 1
  foreach ind $lsx {
    if { [== $first_time 1] } {
      set res $ind
      set first_time 0
    } else {
      set res "$res$mid$ind"
    }
  }
  return $res
}

assertStrEq "1+2+3+4+5+6" [join $misc +]

assertStrEq "wombat" [string tolower "WOMBAT"]
assertStrEq "CALCULUS" [string toupper "calculus"]
assertStrEq "hello" [string trim "  hello  "]

proc expr { a1 args } { 
  if { != [llength $args] 0 } {  
    eval "[lindex $args 0] $a1 [lindex $args 1]"
  } else {
    eval "expr $a1"
  }
}

assertEq 8 [expr 4 + 4]
assertEq 8 [expr {4 + 4}]

set babytime 444
assertEq 444 [set babytime]

assertEq 1 [catch { puts "$thisdoesntexist" }]
assertEq 0 [catch { [+ 1 1] }]

set whagganog ""
proc testglobal {bah} {
  global whagganog
  append whagganog $bah
  return $whagganog
}

assertStrEq 1 [testglobal 1]
assertStrEq 12 [testglobal 2]

set { shh.. ?} 425
assertStrEq " 425 " " ${ shh.. ?} "

assertStrEq "whee $ stuff" "whee \$ stuff"

assertStrEq "whee \$ stuff" "whee \$ stuff"
assertStrEq "whee \$\" stuff" "whee $\" stuff"

puts ""
puts "Done. Passed $assertcount checks."
