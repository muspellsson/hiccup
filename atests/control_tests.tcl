
test "for loop 2" {
  set val 0
  for {set x 1} {$x < 10} {incr x} {
    set val $x
  } 
  checkthat $val == 9
  checkthat $x == 10

  for {set x 1} {$x < 10} {incr x} {
    break
  } 
  checkthat $x == 1

  set val -1
  for {set x 1} {$x < 10} {incr x} {
    continue
    set val $x
  } 
  checkthat $val == -1
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

test "foreach misc" {
  set misc { 1 2 3 4 5 6 }
  proc join2 { lsx mid } {
    set res ""
    set first_time 1
    foreach ind $lsx {
      if { $first_time == 1 } {
        set res $ind
        set first_time 0
      } else {
        set res "$res$mid$ind"
      }
    }
    return $res
  }

  checkthat [join2 $misc +] eq "1+2+3+4+5+6"
  finalize { proc join2 }
}

test "foreach with ns index" {
    namespace eval boo {
    }

    set last 0
    foreach boo::x [list 1 2 3] {
        set last $::boo::x
    }

    checkthat $::boo::x == 3
    finalize { ns boo }
}

test "for loop" {
  set res 0
  for {set i 0} { $i < 20 } { incr i } {
    incr res $i
  }

  checkthat $res == 190
  checkthat $i == 20

  set val 0
  for {set i 20} { $i > 0 } { decr i } {
    incr val
  }

  checkthat $val == 20
}
test "switch" {
  set x 4
  switch $x {
    1 { ::testlib::fail "bad switch" }
    3 { ::testlib::fail "bad switch" }
    4 { ::testlib::pass }
  }
}

test "switch fallthrough" {
  set x 4
  switch $x {
    1 { testlib::fail "bad switch" }
    3 { testlib::fail "bad switch" }
    4 -
    5 { ::testlib::pass }
    4 { testlib::fail "bad switch" }
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
    1 { ::testlib::fail "bad switch" }
    3 { testlib::fail "bad switch" }
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
    2 { testlib::fail "bad Switch" }
    4 { testlib::pass }
    default { testlib::fail "bad Swich" }
  }
}

test "switch arg parse" {
    assert_err {
        switch -glob -exact x { 
            x { testlib::fail "bad" }
        }
    }

    assert_err {
        switch -glob -glob x { 
            x { testlib::fail "bad" }
        }
    }

    assert_noerr {
        switch -glob -- x { 
            x { set x 4 }
        }
    }

    switch -exact -- -glob {
        -glob { testlib::pass }
        default { testlib::fail "didn't match -glob" }
    }
}

test "switch -glob -- --" {
    switch -glob -- -- {
        ++ { testlib::fail "bad match" }
        ?? { testlib::pass }
        default { testlib::fail "bad match" }
    }
}

test "switch glob" {
  set x been
  switch -glob $x {
    2 { testlib::fail "bad Switch1" }
    b*d { testlib::fail "bad Switch2" }
    b??n { testlib::pass }
    been { testlib::fail "wtf" }
    default { testlib::fail "bad Switch3" }
  }
}

test "switch --" {
  set x 4
  switch -- $x {
    2 { testlib::fail "bad Switch" }
    4 { testlib::pass }
    default { testlib::fail "bad Swich" }
  }
}
