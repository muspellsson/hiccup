set assertcount 0
set current_test "Test"

proc die s {
  puts $s
  exit
}

source include.tcl

proc assertEq {a b} {
  global current_test
  if {== $a $b} {
    assertPass
  } else {
    die "$current_test failed: $a != $b"
  }
}

proc assertPass {} {
  puts -nonewline "."
  incr ::assertcount
}

proc assertFail why {
  global current_test
  die "'$current_test' failed: $why"
}


proc checkthat { var op r } {
  set res [$op $var $r]
  if { == $res 1 } {
    assertPass
  } else {
    assertFail "\"$var $op $r\" was not true"
  }
}


proc assertStrEq {a b} {
  if {eq $a $b} {
    assertPass
  } else {
    assertFail "\"$a\" != \"$b\""
  }
}

proc assertNoErr code {
  set ret [catch $code]
  if { == $ret 0 } {
    assertPass
  } else {
    assertFail "code failed: $code"
  }
}

proc assertErr code {
  set ret [catch "uplevel $code"]
  if { == $ret 1 } {
    assertPass
  } else {
    assertFail "code should've failed: $code ($ret)"
  }
}

proc get_err code { 
  catch $code errstr
  return $errstr
}

proc announce { } { 
  puts "Running tests"
}

proc assert code {
  set ret [uplevel $code]
  if { == $ret 1 } { assertPass } else { assertFail "Failed: $code" }
}

announce

assertEq [eval {* 4 4}] 16


proc test {name body} {
  set ::current_test $name
  uplevel "proc test_proc {} {$body}"
  set ret [catch { uplevel test_proc } retval]
  if { == $ret 1 } { puts "Err in test {$name}: $retval" }
}

proc with_test {tn code} {
  global current_test
  set old_test $current_test
  set current_test "$old_test -> $tn"
  uplevel $code
  set current_test $old_test
}

test "upvar" {

  proc uptest {var v} {
    upvar $var loc
    set loc $v
  }

  set x 4
  uptest x 3
  checkthat $x == 3

  proc uptest2 {var2 v} {
    proc inner {a b} {
      upvar 2 $b whee 
      set whee $a
    }
    upvar $var2 lark
    inner $v $var2 
    incr lark
  }

  set y 99 
  uptest2 y 3
  assertEq $y 4
}

test "upvar create" {
  proc xxx {} { upvar up local; set local 5 }
  checkthat [info exists up] == 0
  xxx
  checkthat $up == 5

  proc xxx2 {} { upvar up2 local }
  checkthat [info exists up2] == 0
  xxx2
  checkthat [info exists up2] == 0
}

test "unevaluated blocks aren't parsed" {
  if {== 3 4} {
   "This should be no problem. $woo_etcetera.; 
   "
  } else {
   assertPass
  }
}

test "unused args" {
  proc addem {a b} {
    return [+ $a $a]
    return [+ $b $a]
  }

  checkthat [addem 5 "balloon"] == 10
}

test "incr test" {
  set count 0

  incr count
  incr count
  incr count

  assertEq $count 3

  incr count 2

  assertEq 5 $count

  incr count -2

  assertEq 3 $count

  decr count
  decr count
  decr count

  assertEq $count 0
}

test "math test" { 
  checkthat [pow 2 2] == 4
  checkthat [pow 2 10] == 1024
}

test "list test" {
  set bean [list 1 2 3 4 5 {6 7 8}]

  assertEq [llength $bean] 6
  assertEq [lindex $bean 3] 4
  checkthat [lindex $bean 5] eq {6 7 8}

  checkthat [llength "peanut"] == 1
  checkthat [llength "peanut ontology"] == 2
  checkthat [llength ""] == 0

  checkthat [llength {one [puts bean]}] == 3

  checkthat [llength {a b # c d}] == 5

  checkthat [llength [list [list 1 2 3] [list 3 4 5]]] == 2
  assertEq [lindex 4] 4
  checkthat [lindex $bean 8] eq "" 

  set boo [list {} {} {} {}]
  checkthat [llength $boo] == 4
}


test "lappend" {
  set x {}
  lappend x 1
  lappend x 2 3 "entropy kills"
  checkthat [llength $x] == 4
  checkthat $x eq "1 2 3 {entropy kills}"
}

test "test if, elseif, else" {
  if { eq "one" "two" } {
    die "Should not have hit this."
  } elseif { == 1 1 } {
    assertPass
  } else {
    die "Should not have hit this."
  }
}

test "test args parameter" {
  set total 0
  proc argstest {tot args} {
    upvar $tot total
    set i 0
    while {< $i [llength $args]} {
      set total [+ [lindex $args $i] $total]
      incr i
    }
  }

  assertEq 0 $total
  argstest total 1 2 3 4 5 6 7 8
  assertEq 36 $total
}

test "basic control flow" {
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

  checkthat $sval == 8
  checkthat $sval2 == 4
}



test "string methods" {
  assertEq 4 [string length "five"]
  assertEq 0 [string length ""]
  assertEq 7 [string length "one\ntwo"]
  assertEq 4 [string length "h\n\ti"]

  set fst [string index "whee" 1]
  assertStrEq "h" $fst

  assertStrEq "wombat" [string tolower "WOMBAT"]
  assertStrEq "CALCULUS" [string toupper "calculus"]
  assertStrEq "hello" [string trim "  hello  "]


  checkthat [string reverse "123"] eq "321"
  checkthat [string reverse ""] eq ""
  checkthat [string reverse "X Y"] eq "Y X"
}

test "string index" {
  set fst [string index "whee" 1]
  checkthat $fst eq "h"

  checkthat [string index "what" end] eq "t"

  checkthat [string index "" end] eq ""
  checkthat [string index "hi" 10] eq ""
}

test "string match" {
  checkthat [string match aa aa] == 1
  checkthat [string match aa ab] == 0
  checkthat [string match "WOW" "wow"] == 0
  checkthat [string match -nocase "WOW" "wow"] == 1

  checkthat [string match "a*e" "awesome"] == 1
  checkthat [string match "?arry" "Larry"] == 1
  checkthat [string match "?arry" "Larr?"] == 0
  checkthat [string match "L??ry" "Leary"] == 1
}


test "test append" {
  set somestr "one"
  append somestr " two" " three" " four"
  assertStrEq "one two three four" $somestr
  append avar a b c
  assertStrEq "abc" $avar
}

test "foreach" {
  set numbers {1 2 3 4 5}
  set result 0
  foreach number $numbers {
    set result [+ $number $result]
  }

  assertEq 15 $result

  set fer "old"
  foreach feitem {"a b" "c d"} {
    set fer $feitem
  }

  checkthat $fer eq "c d" 
}

test "foreach multi-bind" {
  foreach {x y z} [list 1 2 3] {}
  checkthat $x == 1
  checkthat $y == 2
  checkthat $z == 3
  foreach {x y z} [list 1 2 3 4] {}
  checkthat $x == 4
  checkthat $y eq ""
  checkthat $z eq ""
}

test "foreach break and continue" {
  set v 0
  foreach x { 1 2 3 4 5 } {
    set v $x
    continue
    set v 0
  } 
  checkthat $v == 5

  set v 0
  foreach x { 1 2 3 4 5 } {
    set v $x
    break
  } 
  checkthat $v == 1
}

test "join and foreach" {
  set misc { 1 2 3 4 5 6 }
  proc join { lsx mid } {
    set res ""
    set first_time 1
    foreach ind $lsx {
      if { == $first_time 1 } {
        set res $ind
        set first_time 0
      } else {
        set res "$res$mid$ind"
      }
    }
    return $res
  }

  checkthat [join $misc +] eq "1+2+3+4+5+6"
}

test "for loop" {
  set res 0
  for {set i 0} { < $i 20 } { incr i } {
    incr res $i
  }

  checkthat $res == 190
  checkthat $i == 20

  set val 0
  for {set i 20} { > $i 0 } { decr i } {
    incr val
  }

  checkthat $val == 20
}

test "empty eval" {
  assertNoErr { eval "" }
  assertNoErr { eval " " }
}

proc expr { a1 args } { 
  if { != [llength $args] 0} {  
    eval "[lindex $args 0] $a1 [lindex $args 1]"
  } else {
    eval "expr $a1"
  }
}

assertEq 8 [expr 4 + 4]
assertEq 8 [expr {4 + 4}]

test "set returns correctly" {
  set babytime 444
  checkthat [set babytime] == 444
  assertEq 512 [set babytime 512]
  assertEq 512 $babytime
}

test "errors and catch" {

  assertErr { error "oh noes" }

  assertEq 1 [catch { puts "$thisdoesntexist" }]
  assertEq 0 [catch { + 1 1 }]

  catch { set x [+ $x 4] } reason
  checkthat $reason eq {can't read "x": no such variable}

  set x 4
  catch { incr x } result
  checkthat $result == 5
}


test "whitespace escaping" {
  set x \
   13

  checkthat $x == 13

  set boo \ redrum

  checkthat $boo eq " redrum"

  set lala \ 
  checkthat $lala eq " "
}

test "int parsing" {
  checkthat 1 != "1 2 3 4"
}


set whagganog ""
set otherthing ""
test "global test" {
    upvar otherthing ot
    proc testglobal {bah} {
      proc modother { m } {
        global whagganog otherthing
        set otherthing $whagganog$m
      }

      global whagganog
      append whagganog $bah
      modother $bah
      return $whagganog
    }

    checkthat [testglobal 1] == 1
    checkthat $ot            == 11
    checkthat [testglobal 2] == 12
    checkthat $ot            == 122
}



test "parsing corners" {
  assertEq 10 [+ 15 -5] # Check that negatives parse.

  set { shh.. ?} 425
  assertStrEq " 425 " " ${ shh.. ?} "

  assertStrEq "whee $ stuff" "whee \$ stuff"

  assertStrEq "whee \$ stuff" "whee \$ stuff"
  assertStrEq "whee \$\" stuff" "whee $\" stuff"
  assertNoErr { 
    if { == 3 3 } { } else { die "bad" } 
  }

  set x four 
  checkthat "$x: 4" eq "four: 4"
}



proc not v {
  if { == 1 $v } { return false } else { return true }
}

test "equality of strings and nums" {
  set x 10
  set y " 10 "
  assert { == $x $y }
  assert { ne $x $y }
  assert { eq 33 33 }
  assert { == "cobra" "cobra" }
  checkthat " 1 " ne 1 
  checkthat " 1 " == 1 
  assert { eq "cobra" "cobra" }
  assert { == 4 4 }
}

test "equality 2" {
  checkthat [list 1 2] eq "1 2"
  checkthat [ne [list 1 2] {1 2}] eq 0
}

test "array reset no-no" {
  set x(1) 44
  checkthat $x(1) == 44
  assertErr { set x 2 }
}

test "early return" {
  set moo 4
  proc yay {} { 
    upvar moo moo2
    return 
    set moo2 5
  }

  yay
  checkthat $moo == 4
}


test "arg count check" {
  proc blah {a b} {
   + $a $b
  }


 assertErr { blah 4 }
 assertErr { blah 4 5 6 }
 assertNoErr { blah 4 5 }

  proc blah2 {a b args} {
    + $a [+ $b [llength $args]]
  }

 assertErr { blah2 1 }
 assertErr { blah2 }
 checkthat [blah2 1 2 3] == 4
 checkthat [blah2 1 2]   == 3
 checkthat [blah2 1 2 1 1 1] == 6
}

test "bad continue/break test" {
  proc whee {} {
    break
  }

  assertErr { whee }

  proc whee2 {} {
    continue
  }

  assertErr { whee2 }

  proc whee3 {} {
    return
  }

  assertNoErr { whee3 }

}

test "incomplete parse" {
  assertErr { set bean 4 " }
  assertNoErr { set bean 4() }
  assertErr { " }
}


test "default proc args" {

  proc plus { t { y 1 } } {
    + $t $y
  }

  proc plus2 { x {y 1} } {
    + $x $y
  }

  proc plus3 { " a 5 " "b 1" } {
    + $a $b
  }

  proc weirdorder { { a1 "boo" } a2 } {
    return $a1$a2
  }

  proc withargs { i {j 4} args } {
    + [llength $args] [+ $i $j]
  }

  checkthat [plus 3 3] == 6
  checkthat [plus2 3 3] == 6
  checkthat [plus3 3 3] == 6
  checkthat [plus 3] == 4
  checkthat [plus2 3] == 4
  checkthat [plus3 3] == 4

  checkthat [plus3] == 6

  checkthat [weirdorder "xx" "yy"] eq "xxyy"

  assertErr { weirdorder "xx" }

  checkthat [withargs 1] == 5
  checkthat [withargs 1 3] == 4
  checkthat [withargs 1 3 1] == 5
  checkthat [withargs 1 3 1 1 1 8 1] == 9
}

test "lone subcommand" {
  proc id {x} { return $x }
  set x 0

  [id "set"] x 11

  checkthat $x == 11
}

test "info" {
  checkthat [info exists x] == 0
  set x 4
  checkthat [info exists x] == 1
  checkthat [info exists current_test] == 0
  global current_test
  checkthat [info exists current_test] == 1
  # TODO: Check upvar'd exists
}

test "unset" {
  set y 4
  checkthat $y == 4
  checkthat [info exists y] == 1
  checkthat [unset y] eq ""
  assertErr { incr y }
  checkthat [info exists y] == 0
}

test "unset with upvar" {
  proc unset_x {} { 
    with_test "in proc" {
      upvar x boo 
      checkthat [info exists boo] == 1
      checkthat $boo == 0
      unset boo 
      checkthat [info exists boo] == 0
    }
  }
  set x 0
  checkthat $x == 0
  checkthat [info exists x] == 1
  unset_x
  checkthat [info exists x] == 0
}

test "info level" {
  checkthat [uplevel {info level}] == 0
  checkthat [info level] == 1
  proc getlevel {} {
    return [info level]
  }

  checkthat [getlevel] == 2
}

test "array set/get" {
  set boo(4) 111
  checkthat "$boo(4)" == 111
  set boo(5) 112
  checkthat $boo(5) == 112

  set boo(wallaby) "next tuesday"
  checkthat $boo(wallaby) eq "next tuesday"

  set "boo( oh no! )" 4
  checkthat "$boo( oh no! )" == 4

  proc succ {v} { return [+ $v 1] }
  set i 0
  set arr($i) 0
  set "arr([succ [succ $i]])" 1
  checkthat "$arr([succ [succ $i]])" == 1
}


test "array size" {
  checkthat [array size boo] == 0
  set boo(0) 1
  set boo(1) 5
  set boo(2) 2

  checkthat [array size boo] == 3
}

test "array exists" {
  checkthat [array exists arr1] == 0

  set notarr 44
  checkthat [array exists notarr] == 0

  set arr1(0) 1

  checkthat [array exists arr1] == 1
}

test "array vs scalar" {
  assertErr {
    set x 4
    set x(1) 4
  }
}

test "array set" {
  assertErr { 
    array set arr { x 1 y 2 z }
  }

  array set arr {
    1 one
    2 two
    3 three
    4 four
  }

  checkthat $arr(1) eq one
  checkthat $arr(2) eq two
  checkthat $arr(3) eq three
  checkthat $arr(4) eq four
}

test "array get" {
  array set arr { x "1 2" y {2 3} "handy man" 3 }

  checkthat [llength [array get arr]] == 6
}

test "proc must be complete" {
  assertErr { proc banana }
  assertErr { proc banana { puts "banana" } }
  assertNoErr { proc banana { } { puts "banana" } }
  assertNoErr { proc banana {} { puts "banana" } }
}


test "rename" {
  assertErr { rename one one_ }
  proc one {} { return 1 }
  checkthat [one] == 1
  rename one one_
  assertErr { one }
  checkthat [one_] == 1
  rename one_ ""
  assertErr { one_ }
}

test "for loop 2" {
  set val 0
  for {set x 1} {< $x 10} {incr x} {
    set val $x
  } 
  checkthat $val == 9
  checkthat $x == 10

  for {set x 1} {< $x 10} {incr x} {
    break
  } 

  checkthat $x == 1

  set val -1
  for {set x 1} {< $x 10} {incr x} {
    continue
    set val $x
  } 
  checkthat $val == -1
}

proc ignore _ { return {} }

ignore {
  test "global ns proc" {
    # not yet
    checkthat [::+ 1 1] == 2
    checkthat [+ 1 1] == 2
  }
}

test "switch" {
  set x 4
  switch $x {
    1 { assertFail "bad switch" }
    3 { assertFail "bad switch" }
    4 { assertPass  }
  }
}

test "switch fallthrough" {
  set x 4
  switch $x {
    1 { assertFail "bad switch" }
    3 { assertFail "bad switch" }
    4 -
    5 { assertPass }
    4 { assertFail "bad switch" }
  }

  set val 0
  switch $x {
    1 - 2 - 3 - 4 - 5 { incr val }
  }
  checkthat $val == 1
}

test "switch default" {
  set res "failed"
  set x 4
  switch $x {
    1 { assertFail "bad switch" }
    3 { assertFail "bad switch" }
    default { set res "worked" }
  }

  checkthat $res eq "worked"
}

test "switch return" {
  set x 2
  set result \
    [switch $x {
      1 { + 1 2 }
      2 { + 2 2 }
      3 { + 3 2 }
    }]

 checkthat $result == 4

}

test "list escaping" {
  set x [list 1 2 3 \{ \} 6]
  checkthat [llength $x] == 6
}

test "split" {
  set res [split "one two three four"]
  checkthat [llength $res] == 4

  set res [split "comp.lang.misc.fuzzyx" ".x"]
  checkthat [lindex $res 2] eq "misc"
  checkthat [llength $res] == 5


  checkthat [llength [split "Hey you" {}]] == 7
}

test "namespaces" {
  set ::x 4
  checkthat $::x == 4
  unset ::x
  assertErr { checkthat $::x == 4 }
}

test "exists and unset global" {
  set ::gv 9
  checkthat [info exists ::gv] == 1
  unset ::gv
  checkthat [info exists ::gv] == 0
}

test "incr global" {
  set ::gv 1
  incr ::gv
  checkthat $::gv == 2
  unset ::gv
}

test "set global" {
  set ::x 4
  checkthat [set ::x] == 4
  unset ::x
}


test "unknown" {
  set oops 0
  set missed {}
  rename banana ""
  proc unknown {name args} {
    uplevel {incr oops}
    uplevel "set missed $name"
  }
  checkthat $oops == 0
  banana 44
  checkthat $oops == 1
  checkthat $missed eq banana
  rename unknown ""
}

test "uplevel issue" {
  set level [info level]
  catch { uplevel { error "Oh no" } }
  checkthat [info level] == $level
}

set a_global 9
test "global namespace" {
  checkthat [info exists a_global] == 0
  checkthat $::a_global == 9
}

puts ""
puts stdout "Done. Passed $assertcount checks."
