if { !("==" in [info commands]) } {
  namespace import ::tcl::mathop::*
  namespace import ::tcl::mathfunc::*
}

namespace eval testlib {
  variable current_test "Test"
  variable trace_test 0
  variable tests
  variable passcount 0
  variable assertcount 0
  variable ignorecount 0
}

proc ::testlib::ignore {} {
    incr ::testlib::ignorecount
    return -code return
}

proc ::testlib::pass {} {
  puts -nonewline "."
  incr ::testlib::assertcount
  incr ::testlib::passcount
}

proc ::testlib::fail why {
  variable ::testlib::current_test
  incr ::testlib::assertcount
  puts -nonewline "\n'$current_test' failed: $why"
}

proc verify { code { msg "" } } {
  if { [uplevel $code] } {
    if { $::testlib::trace_test == 1 } { puts "\"$code\" was true" }
    ::testlib::pass
  } else {
    ::testlib::fail "\"$code\" was not true ($msg)"
  }
}

proc checkthat { var { op == } { r 1 } { msg "" } } {
  set res [$op $var $r]
  if { $res == 1 } {
    if { $::testlib::trace_test == 1 } { puts "\"$var $op $r\" was true" }
    ::testlib::pass
  } else {
    ::testlib::fail "\"$var $op $r\" was not true ($msg)"
  }
}

proc assertNoErr code {
  set ret [catch "uplevel {$code}"]
  if { $ret == 0 } {
    ::testlib::pass
  } else {
    ::testlib::fail "code failed: $code"
  }
}

proc assert_noerr code {
  uplevel [list assertNoErr $code]
}

proc assertErr { code { msg "" } } {
  set ret [catch "uplevel {$code}"]
  if { $ret == 1 } {
    ::testlib::pass
  } else {
    ::testlib::fail "code should've failed: $code ($ret) $msg"
  }
}

proc assert_err { code { msg "" } } {
  uplevel [list assertErr $code $msg]
}

proc get_err code { 
  catch $code errstr
  return $errstr
}

proc assert code {
  set ret [uplevel $code]
  if { $ret == 1 } { ::testlib::pass } else { ::testlib::fail "Failed: $code" }
}

proc test {name body} {
  if [info exists ::testlib::tests($name)] {
      puts stderr "WARNING: Already a test called \"$name\""
  }
  set ::testlib::tests($name) $body
  set ::testlib::test_results($name) "not run"
}

proc ::testlib::run_tests {} {
  puts "Running [array size testlib::tests] tests."
  foreach tn [array names ::testlib::tests] {
    uplevel "::testlib::run_test {$tn}"
  }
  puts stdout "\nDone. Passed $::testlib::passcount / $::testlib::assertcount checks."
  if {$::testlib::ignorecount > 0} {
      puts "($::testlib::ignorecount ignored)"
  }
}

proc ::testlib::run_test tname {
  uplevel "proc test_proc {} {$::testlib::tests($tname)}"
  set ::testlib::current_test $tname
  set ret [catch { uplevel test_proc } retval]
  if { $ret == 1 } { ::testlib::fail "Error in test {$tname}: $retval" }
  puts -nonewline "|"
  rename test_proc {}
}

proc with_test {tn code} {
  variable ::testlib::current_test
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
      interp -
      int { interp delete $name }
      default { error "Unrecognized type: $type" }
    }
  }
}

proc not v {
  ! $v
}
