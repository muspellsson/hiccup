set assertcount 0
namespace eval testlib {
  variable current_test "Test"
  variable trace_test 0
  variable tests
}

proc die s {
  puts $s
  exit
}

proc assertPass {} {
  puts -nonewline "."
  incr ::assertcount
}

proc assertFail why {
  variable testlib::current_test
  die "'$current_test' failed: $why"
}


proc checkthat { var { op == } { r 1 } } {
  set res [$op $var $r]
  if { == $res 1 } {
    if { == $::testlib::trace_test 1 } { puts "\"$var $op $r\" was true" }
    assertPass
  } else {
    assertFail "\"$var $op $r\" was not true"
  }
}

proc assertNoErr code {
  set ret [catch "uplevel {$code}"]
  if { == $ret 0 } {
    assertPass
  } else {
    assertFail "code failed: $code"
  }
}

proc assertErr code {
  set ret [catch "uplevel {$code}"]
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

proc assert code {
  set ret [uplevel $code]
  if { == $ret 1 } { assertPass } else { assertFail "Failed: $code" }
}

proc test {name body} {
  set testlib::current_test $name
  set testlib::tests($name) $body
  uplevel "proc test_proc {} {$body}"
  set ret [catch { uplevel test_proc } retval]
  if { == $ret 1 } { assertFail "Error in test {$name}: $retval" }
}

proc with_test {tn code} {
  variable testlib::current_test
  set old_test $current_test
  set current_test "$old_test -> $tn"
  uplevel $code
  set current_test $old_test
}

proc finalize { items { code {} } } {
  uplevel $code
  foreach { type name } $items {
    switch $type {
      ns -
      namespace { namespace delete $name }
      proc -
      pr { rename $name {} }
      default { error "Unrecognized type: $type" }
    }
  }
}

proc not v {
  if { == 1 $v } { return 0 } else { return 1 }
}
