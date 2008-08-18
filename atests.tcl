
source include.tcl
source testlib.tcl

source atests/core_tests.tcl
source atests/control_tests.tcl
source atests/string_tests.tcl
source atests/list_tests.tcl
source atests/array_tests.tcl
source atests/expr_tests.tcl
source atests/ns_tests.tcl
source atests/interp_tests.tcl
source atests/event_tests.tcl

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

  finalize { 
    proc uptest 
    proc uptest2 
    proc inner }
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

  finalize { proc xxx proc xxx2 }
}

test "error on bad upvar level" {
  assert_err { upvar 1000 x x }
}

test "unevaluated blocks aren't parsed" {
  if {3 == 4} {
   "This should be no problem. $woo_etcetera.; 
   "
  } else {
   ::testlib::pass
  }
}


test "unused args" {
  proc addem {a b} {
    return [+ $a $a]
    return [+ $b $a]
  }

  checkthat [addem 5 "balloon"] == 10

  finalize { proc addem }
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

test "incr creates" {
  checkthat [info exists fooz] == 0
  assert_noerr { incr fooz }
  checkthat $fooz == 1
}

test "math test" { 
  checkthat [pow 2 2] == 4
  checkthat [pow 2 10] == 1024

  checkthat [expr { pow(2,-1) }] == 0.5
  checkthat [expr { int(3.5) }] eq 3
  checkthat [expr { int(3) }] eq 3

  checkthat [expr { sqrt(2 + 2) }] eq 2.0
  checkthat [+ 3.5 3.5] == 7.0

  checkthat [* 2 1.5] == 3.0

  checkthat [+ 1 1 1 1 1 1] == 6
  checkthat [* 1 1 1 1 1 1 2] == 2
}

test "double compare" {
  checkthat [< 0.3 0.9] 
  checkthat [<= 0.3 0.9] 
  checkthat [<= 0.9 0.9]
  checkthat [<= 0.9 0.3] == 0 
  checkthat [> 1.9 1] 
  checkthat [== 3.8 3.8]

  checkthat [expr { double(3) }] == 3.0
}


test "test if, elseif, else" {
  if { "one" eq "two" } {
    ::testlib::fail "Should not have hit this."
  } elseif { 1 == 1 } {
    ::testlib::pass
  } else {
    testlib::fail "Should not have hit this."
  }
}

test "test args parameter" {
  set total 0
  proc argstest {tot args} {
    upvar $tot total
    set i 0
    while { $i < [llength $args]} {
      set total [+ [lindex $args $i] $total]
      incr i
    }
  }

  checkthat $total == 0
  argstest total 1 2 3 4 5 6 7 8
  checkthat $total == 36

  finalize { proc argstest }
}

test "basic control flow" {
  set sval 0
  set sval2 1
  while { $sval <= 10} {
    incr sval
    if { 8 <= $sval} {
      break
    }
    if { 4 <= $sval} {
      continue
    }
    incr sval2
  }

  checkthat $sval == 8
  checkthat $sval2 == 4
}

test "test append" {
  set somestr "one"
  append somestr " two" " three" " four"
  checkthat $somestr eq "one two three four"
  append avar a b c
  checkthat $avar eq "abc"
}


test "eval tests" {
  checkthat [eval {* 4 4}] == 16

  assert_noerr { eval "" }
  assert_noerr { eval " " }

  assert_err { eval } "Eval needs 1 or more args"

  checkthat [eval * 4 4] == 16 {Eval concats multiple args}
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


test "catching return, break and continue" {
  checkthat [catch {set x 4}] == 0
  checkthat [catch {error ERR}] == 1
  checkthat [catch return] == 2
  checkthat [catch break] == 3
  checkthat [catch continue] == 4
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
    finalize { proc modother proc testglobal }
}
unset whagganog
unset otherthing


test "parsing corners" {
  checkthat [+ 15 -5] == 10  {Check that negatives parse}

  set { shh.. ?} 425
  checkthat " 425 " eq " ${ shh.. ?} "

  checkthat "whee $ stuff" eq "whee \$ stuff"

  checkthat "whee \$ stuff" eq "whee \$ stuff"
  checkthat "whee \$\" stuff" eq "whee $\" stuff"
  assertNoErr { 
    if { 3 == 3 } { } else { testlib::fail "bad" } 
  }

  set x four 
  checkthat "$x: 4" eq "four: 4"

  # comment \
  testlib::fail { should be still a comment }
  testlib::pass 
}

test "ns parsing with array lookup" {
  assert_noerr {
    set x(::) 4
    set y $x(::)
  }
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

test "early return" {
  set moo 4
  finalize { proc yay } {
    proc yay {} { 
      upvar moo moo2
      return 
      puts "WTF SHOULD NOT HAPPEN"
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

 assert_err { blah2 1 }
 assert_err { blah2 }
 checkthat [blah2 1 2 3] == 4
 checkthat [blah2 1 2]   == 3
 checkthat [blah2 1 2 1 1 1] == 6

 finalize { proc blah proc blah2 }
}

test "proc optional arg, single default" {
 assert_noerr { 
   proc blah {a {b {3 4}} } {}
 }

 assert_err { 
   proc blah {a {b 3 4} } {}
 }

 finalize { proc blah }
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

  finalize { proc whee proc whee2 proc whee3 }
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

  finalize { proc plus proc plus2 proc plus3 proc weirdorder proc withargs }
}

test "lone subcommand" {
  proc id {x} { return $x }
  set x 0

  [id "set"] x 11

  checkthat $x == 11
}


test "unset" {
  set y 4
  checkthat $y == 4
  checkthat [info exists y] == 1
  checkthat [unset y] eq ""
  assert_err { set v $y }
  checkthat [info exists y] == 0 { y exists }
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

  finalize { proc unset_x }
}

test "unset array elt" {
  set x(4) 4
  set x(5) 5
  checkthat [array size x] == 2
  checkthat $x(4) == 4
  unset x(4)

  checkthat [array size x] == 1
}

test "unset multiple" {
    set x 1
    set y 2
    set z 3

    foreach vn {x y z} {
        checkthat [info exists $vn]
    }

    unset x y z

    foreach vn {x y z} {
        checkthat [info exists $vn] == 0
    }
}

test "proc must be complete" {
  assertErr { proc banana }
  assertErr { proc banana { puts "banana" } }
  assertNoErr { proc banana { } { puts "banana" } }
  assertNoErr { proc banana {} { puts "banana" } }
  finalize { proc banana }
}


test "rename" {
  assert_err { rename one one_ }
  proc one {} { return 1 }
  checkthat [one] == 1
  rename one one_
  assert_err { one }
  checkthat [one_] == 1
  rename one_ ""
  assert_err { one_ }
}

test "global ns proc" {
  checkthat [::+ 1 1] == 2
  checkthat [+ 1 1] == 2
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

test "uplevel arg" {
  proc in_a_proc {} {
    assert_noerr { uplevel set x 10 }
  }

  in_a_proc 
  checkthat $x == 10

  finalize { proc in_a_proc }
}

test "global namespace" {
  uplevel { set a_global 9 }
  checkthat [info exists a_global] == 0
  checkthat $::a_global == 9
  uplevel { unset a_global }
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

test "procs declared in namespace global" {
  proc ::woot {} { return 4545 }
  checkthat [::woot] == 4545
  finalize { proc woot }
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
  checkthat [namespace exists foo] == 0
  assert_err {
    proc foo::bar {} { return 1 }
  }

  assert_err {
    proc ::foo::bar {} { return 1 }
  }

  namespace eval foo {}
  assert_noerr {
    proc foo::bar {} { return 1 }
  }

  finalize { namespace foo }
}

test "rename with ns qualifiers" {
  namespace eval foo {
    proc bar {} { return "omg!" }
  }

  assert_noerr { ::foo::bar }

  rename ::foo::bar ::foo::baz

  assert_err { ::foo::bar }
  checkthat [::foo::baz] eq "omg!"

  rename ::foo::baz whatnot
  assert_err { ::foo::baz }

  checkthat [whatnot] eq "omg!"

  finalize { namespace foo proc whatnot }
}

test "uplevel in ns" { 
  set g 0
  namespace eval foo { 
    uplevel { set g [namespace current] }
  }
  
  checkthat $g eq {::}
  finalize { ns foo }
}

test "globally qualified proc in ns" {
  namespace eval foo { 
    proc ::blah {} { return 4 }
  }

  assertErr { ::foo::blah }
  assertNoErr { ::blah; blah }

  assertNoErr {
    namespace eval foo {
      rename ::blah {}
    }
  }
  assert_err { ::blah }
  finalize { ns foo } 
}

test "list eval" {
  checkthat [eval [list * 3 5]] == 15
}

test "odd procs" {
  set one [+ 1 0]
  proc $one {} { return ONE }
  checkthat [eval $one] eq ONE

  set ffff [+ 55 0.55]
  proc $ffff {} { return FFFF }
  checkthat [eval $ffff] eq FFFF
  finalize { proc 1 proc 55.55 }
}


::testlib::run_tests

#puts "([llength [info procs]] lingering procs)"
