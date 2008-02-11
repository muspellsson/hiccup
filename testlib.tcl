set assertcount 0
set current_test "Test"

proc die s {
  puts $s
  exit
}

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


proc checkthat { var { op == } { r 1 } } {
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

proc assert code {
  set ret [uplevel $code]
  if { == $ret 1 } { assertPass } else { assertFail "Failed: $code" }
}

proc test {name body} {
  set ::current_test $name
  uplevel "proc test_proc {} {$body}"
  set ret [catch { uplevel test_proc } retval]
  if { == $ret 1 } { assertFail "Error in test {$name}: $retval" }
}

proc with_test {tn code} {
  global current_test
  set old_test $current_test
  set current_test "$old_test -> $tn"
  uplevel $code
  set current_test $old_test
}

proc finalize { items code } {
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
