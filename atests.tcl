
source include.tcl
source testlib.tcl

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
  checkthat $y == 4
}

test "info vars" {
  checkthat [llength [info vars]] == 0
  set x 4
  checkthat [info vars] eq "x"
  checkthat [info vars y] eq {}
  checkthat [info vars ?] eq "x"
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

test "error on bad upvar level" {
  assertErr { upvar 1000 x x }
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

  checkthat $count == 3

  incr count 2

  checkthat $count == 5

  incr count -2

  checkthat $count == 3

  decr count
  decr count
  decr count

  checkthat $count == 0
}

test "math test" { 
  checkthat [pow 2 2] == 4
  checkthat [pow 2 10] == 1024
}

test "list test" {
  set bean [list 1 2 3 4 5 {6 7 8}]

  checkthat [llength $bean] == 6
  checkthat [lindex $bean 3] == 4
  checkthat [lindex $bean 5] eq {6 7 8}

  checkthat [llength "peanut"] == 1
  checkthat [llength "peanut ontology"] == 2
  checkthat [llength ""] == 0

  checkthat [llength {one [puts bean]}] == 3

  checkthat [llength {a b # c d}] == 5

  checkthat [llength [list [list 1 2 3] [list 3 4 5]]] == 2
  checkthat [lindex 4] == 4
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

  checkthat $total == 0
  argstest total 1 2 3 4 5 6 7 8
  checkthat $total == 36
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
  checkthat 4 == [string length "five"]
  checkthat 0 == [string length ""]
  checkthat 7 == [string length "one\ntwo"]
  checkthat 4 == [string length "h\n\ti"]

  set fst [string index "whee" 1]
  checkthat "h" eq $fst

  checkthat "wombat" eq [string tolower "WOMBAT"]
  checkthat "CALCULUS" eq [string toupper "calculus"]
  checkthat "hello" eq [string trim "  hello  "]


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
  checkthat [string match aa aa]
  checkthat [string match aa ab] == 0
  checkthat [string match "WOW" "wow"] == 0
  checkthat [string match -nocase "WOW" "wow"]

  checkthat [string match "a*e" "awesome"] == 1
  checkthat [string match "?arry" "Larry"] == 1
  checkthat [string match "?arry" "Larr?"] == 0
  checkthat [string match "L??ry" "Leary"] == 1
}


test "test append" {
  set somestr "one"
  append somestr " two" " three" " four"
  checkthat $somestr eq "one two three four"
  append avar a b c
  checkthat $avar eq "abc"
}

test "foreach" {
  set numbers {1 2 3 4 5}
  set result 0
  foreach number $numbers {
    set result [+ $number $result]
  }

  checkthat $result == 15

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

test "eval tests" {
  checkthat [eval {* 4 4}] == 16

  assertNoErr { eval "" }
  assertNoErr { eval " " }
}

test "expr" {
  checkthat [expr 4 + 4] == 8
  checkthat [expr {4 + 4}] == 8
  checkthat [expr {9 + (1 * 1)}] == 10
  set x 10
  checkthat [expr { $x + $x }] == 20
  checkthat [expr { [+ 1 1] * 2 }] == 4
}

test "set returns correctly" {
  set babytime 444
  checkthat [set babytime] == 444
  checkthat [set babytime 512] == 512
  checkthat $babytime == 512
}

test "errors and catch" {

  assertErr { error "oh noes" }

  checkthat [catch { puts "$thisdoesntexist" }] == 1
  checkthat [catch { + 1 1 }] == 0

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
    checkthat $::otherthing  == 11
    checkthat [testglobal 2] == 12
    checkthat $::otherthing  == 122
}
unset whagganog
unset otherthing



test "parsing corners" {
  checkthat [+ 15 -5] == 10  # Check that negatives parse.

  set { shh.. ?} 425
  checkthat " 425 " eq " ${ shh.. ?} "

  checkthat "whee $ stuff" eq "whee \$ stuff"

  checkthat "whee \$ stuff" eq "whee \$ stuff"
  checkthat "whee \$\" stuff" eq "whee $\" stuff"
  assertNoErr { 
    if { == 3 3 } { } else { die "bad" } 
  }

  set x four 
  checkthat "$x: 4" eq "four: 4"
}

test "equality of strings and nums" {
  set x 10
  set y " 10 "
  checkthat $x == $y 
  checkthat $x ne $y 
  checkthat 33 eq 33
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
  finalize { proc yay } {
    proc yay {} { 
      upvar moo moo2
      return 
      set moo2 5
    }

    yay
    checkthat $moo == 4
  }
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

test "info exists" {
  checkthat [info exists x] == 0
  set x 4
  checkthat [info exists x] == 1
  checkthat [info exists current_test] == 0
  variable  testlib::current_test
  checkthat [info exists current_test] == 1

  set arr(4) 2
  checkthat [info exists arr(3)] == 0
  checkthat [info exists arr(4)] == 1
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

test "array names" {
  set arr(0) 1
  set arr(feed) people
  checkthat [llength [array names arr]] == 2
}

test "array names pat" {
  set arr(0) 1
  set arr(feed) people
  checkthat [llength [array names arr oot]] == 0
  checkthat [llength [array names arr ?]] == 1
  checkthat [llength [array names arr ??]] == 0
  checkthat [llength [array names arr f*]] == 1
  checkthat [llength [array names arr f??d]] == 1
}

test "array names mode pat" {
  set arr(0) 1
  set arr(feed) people
  set arr(food) chickens
  assertErr { array names arr -bad 4 }
  checkthat [llength [array names arr -glob ?]] == 1
  checkthat [llength [array names arr -exact ?]] == 0
  checkthat [llength [array names arr -glob ??]] == 0
  checkthat [llength [array names arr -glob f*]] == 2
  checkthat [llength [array names arr -exact food]] == 1
  checkthat [llength [array names arr -glob f??d]] == 2
  checkthat [llength [array names arr -exact f??d]] == 0
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

  assertErr { checkthat $arr(5) eq five }

  array set arr {
    5 five
    3 threee
  }

  checkthat $arr(5) eq five
  checkthat $arr(3) eq threee
}

test "array get" {
  array set arr { x "1 2" y {2 3} "handy man" 3 }
  checkthat [llength [array get arr]] == 6
  checkthat [llength [array get donkey]] == 0
}

test "array get pat" {
  array set arr { x "1 2" y {2 3} "handy man" 3 }
  checkthat [llength [array get arr]] == 6
  checkthat [llength [array get arr x]] == 2
  checkthat [llength [array get arr ?]] == 4
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

test "switch exact" {
  set x 4
  switch -exact $x {
    2 { assertFail "bad Switch" }
    4 { assertPass }
    default { assertFail "bad Swich" }
  }
}

test "switch glob" {
  set x been
  switch -glob $x {
    2 { assertFail "bad Switch1" }
    b*d { assertFail "bad Switch2" }
    b??n { assertPass }
    been { assertFail "wtf" }
    default { assertFail "bad Switch3" }
  }
}

test "switch --" {
  set x 4
  switch -- $x {
    2 { assertFail "bad Switch" }
    4 { assertPass }
    default { assertFail "bad Swich" }
  }
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

test "global namespace" {
  uplevel { set a_global 9 }
  checkthat [info exists a_global] == 0
  checkthat $::a_global == 9
  uplevel { unset a_global }
}

test "namespace current" {
  #checkthat [namespace current] == "::"
  checkthat [namespace parent] == {}
}


test "namespace proc 1" {
  finalize { namespace temp } {
    namespace eval temp {
      proc one {} { return 1 }
      checkthat [namespace current] == "::temp"
      checkthat [namespace parent] == ""
      checkthat [one] == 1
    }

    assertErr { one }

    assertNoErr { ::+ 4 5 }

    checkthat [temp::one] == 1
    checkthat [::temp::one] == 1
  }
}


test "nested ns" {
  namespace eval abc {
    namespace eval xyz {
      proc tada {} { return "yay!" }
    }
  }

  assertErr { tada }
  assertErr { abc::tada }
  assertErr { ::abc::tada }
  assertErr { ::xyz::tada }
  assertErr { xyz::tada }

  checkthat [abc::xyz::tada] eq "yay!"
  checkthat [::abc::xyz::tada] eq "yay!"
}

test "double ns" {
  finalize { ns abc2 } {
    namespace eval abc2::xyz2 {
        proc tada {} { return "yay!" }
    }
    checkthat [abc2::xyz2::tada] eq "yay!"
  }
}

test "namespace exists" { 
  checkthat [namespace exists imaginary] == 0
# not yet
#  checkthat [namespace exists "::"] == 1
  checkthat [namespace exists {}] == 1
}

test "ns level" {
  finalize { namespace abc } {
    set ::lev 0
    namespace eval abc {
      set ::lev [info level]
    }
    
    checkthat [info level] == [- $::lev 1]
    unset ::lev
  }
}

test "simple variable" {
  finalize { namespace foo } {
    namespace eval foo {
      variable wow 99
    }
   checkthat $foo::wow == 99
   checkthat $::foo::wow == 99
   set ::foo::wow 3
   checkthat $foo::wow == 3 
 }
}

test "namespace variable evil" {
  finalize { proc evil ns temp_ns } {
    proc evil {} {
      checkthat $::temp_ns::value == 4
    }

    namespace eval temp_ns {
      variable value 4
      ::evil
    }
  }
}


test "upvar in uplevel" {
  proc set_to_3 vn {
    upvar $vn x
    set x 3
  }

  proc thingee {} { 
    uplevel { set_to_3 y }
  }

  set y 4
  thingee
  checkthat $y == 3
  finalize { proc thingee proc set_to_3 }
}

test "upvar in uplevel 2" {
  proc bind_to_x vn {
    uplevel {
      upvar $vn x
    }
  }

  proc thingee vn { 
    bind_to_x $vn
    set x 3
  }

  set y 4
  thingee y
  checkthat $y == 3

  finalize { proc thingee proc bind_to_x }
}

test "namespace delete" {
  namespace eval foo {
    proc something {} { return 1 }
  }
  checkthat [namespace exists foo]
  namespace delete foo
  checkthat [not [namespace exists foo]]
}

test "ns variable scalar" {
  namespace eval hidden { variable IDS 4 }

  proc getit {} {
    variable ::hidden::IDS
    return $IDS
  }

 proc setit {} {
   variable ::hidden::IDS
   set IDS 11
 }

  checkthat [getit] == 4
  setit
  checkthat [getit] == 11

  finalize { proc setit proc getit namespace hidden }
}

test "ns variable array" {
  namespace eval foo {
    variable farr
    array set farr {
      1 one
      2 two
      3 three
    }

    checkthat $farr(1) eq one
  } 

  proc get_index ind {
    variable ::foo::farr 
    return $farr($ind)
  }
  
  checkthat [get_index 2] eq two
}

test "ns variable undefined scalar" {
  namespace eval hidden { variable IDS }

  proc getit {} {
    variable ::hidden::IDS
    return $IDS
  }

 proc setit {} {
   variable ::hidden::IDS
   set IDS 11
 }

  assertErr { getit }
  setit
  checkthat [getit] == 11

  finalize { proc setit proc getit namespace hidden }
}

test "ns proc variable" {
  namespace eval foo {
    variable boo 10
    proc doit {} {
      checkthat [not [info exists boo]]
      variable boo
      checkthat $boo == 10
      checkthat [info exists boo]
    }
    doit
  }

  finalize { namespace foo }
}

test "ns variable array 'array size'" {
  namespace eval foo {
    variable arr
    array set arr { 1 one 2 two 3 three }
  }

  checkthat [array size ::foo::arr] == 3
  finalize { namespace foo }
}

test "namespace tail" {
  checkthat [namespace tail boo] == boo
  checkthat [namespace tail ::oh::no] == no
  checkthat [namespace tail oh::no] == no
  checkthat [namespace tail ::] == {}
}

test "namespace qualifiers" {
  checkthat [namespace qualifiers boo] == {}
  checkthat [namespace qualifiers ::oh::no] == ::oh
  checkthat [namespace qualifiers oh::no] == oh
  checkthat [namespace qualifiers ::] == {}
}

test "procs declared in namespace global" {
  proc ::woot {} { return 4545 }
  checkthat [::woot] == 4545
}

test "procs declared in foo namespace" {
  namespace eval foo {}
  proc foo::ret5 {} { return 5 }

  checkthat [foo::ret5] == 5
  checkthat [::foo::ret5] == 5

  proc ::foo::ret6 {} { return 6 }

  checkthat [foo::ret6] == 6
  checkthat [::foo::ret6] == 6
  finalize { namespace foo }
}

test "proc in namespace that doesn't exist fails" {
  assertErr {
    proc foo::bar {} { return 1 }
  }

  assertErr {
    proc ::foo::bar {} { return 1 }
  }

  namespace eval foo {}

  assertNoErr {
    proc foo::bar {} { return 1 }
  }

  finalize { namespace foo }
}

run_tests
