test "expr" {
  checkthat [expr 4 + 4] == 8
  checkthat [expr {4 + 4}] == 8
  checkthat [expr {9 + (1 * 1)}] == 10
  set x 10
  checkthat [expr { $x + $x }] == 20
  checkthat [expr { [+ 1 1] * 2 }] == 4

  checkthat [expr { 3 < 4.5 }] 
  checkthat [expr { 3 <= 4.5 }] 
  checkthat [expr { 4.5 <= 4.5 }] 
  checkthat [expr { 4.5 == "4.5" }] 
  checkthat [expr { 4.5 != 4 }] 
  checkthat [expr { 4.9 > 2 }]
  checkthat [expr { 4.9 >= 2 }]
  checkthat [expr { 4.9 >= 4.9 }]
  checkthat [expr { "one" eq "one" }] 
  checkthat [expr { "two" ne "one" }] 
  checkthat [expr { "1" eq 1 }] 

  checkthat [expr {1 + 1 * 2}] == 3
}

test "expr strings escaping" {
    checkthat [expr {{one\ttwo}}] eq {one\ttwo} {block}
    set x [expr { "one\ttwo" }]
    checkthat $x eq "one\ttwo" {string}
}

test "expr str with str" {
    checkthat [expr { "[set x "X"]" }] eq "X"
}

test "expr str with variable substitution" {
    set gxx "WOO"
    set y 11
    checkthat [expr { " $gxx $gxx [incr y]" }] eq " WOO WOO 12"
}

test "string as bool" {
    assert_err { while { "banana" } { } }
}

test "expr fun parse" {
  checkthat [expr { sin(0.0) + 10 }] == 10.0

  checkthat [expr { !(3 == 4) }]
  checkthat [expr { !0 }]
  checkthat [expr { !true }] == 0

  assert_err { expr {} }
  
  assert_noerr { expr {[set x ""]} }

  checkthat [expr { 3 + -(-3) }] == 6
}

test "expr shift" {
  checkthat [expr { 3 << 4 }] == 48
  checkthat [expr { 2 << 0 }] == 2
  checkthat [expr { 1 << 8 }] == 256

  checkthat [expr { 3 >> 2 }] == 0
  checkthat [expr { 3 >> 1 }] == 1

  assert_err { expr { 3 << -2 } }
}

test "expr multi-subexpression" {
    set x 3
    checkthat [expr { [set y 99; set x] + 1 }] == 4
}

test "expr precedence" {
  checkthat [expr { 3 << 1 < 4 }] == 0

  checkthat [expr { 2 ** 3 * 2 }] == 16
}

test "expr in" {
  checkthat [expr { 3 in "1 2 3" }]
  checkthat [expr { !(5 in "1 2 3") }]
}

test "expr function params" {
  checkthat [expr { pow(2,2) }] == 4

  assert_err { expr { rand(44) } }
  assert_noerr { expr { rand() } }

  checkthat [expr { max(1,2,11,4,55) }] == 55
  checkthat [expr { min(1,2,11,4,55) }] == 1
}

test "abs mathfunc" {
  checkthat [expr { abs(-3) }] eq 3
  checkthat [expr { abs(3) }] eq 3
}

test "bool test" {
  checkthat [expr { true || false }] == 1
  checkthat [expr { false || true }] == 1
  checkthat [expr { false || false }] == 0
  checkthat [expr { true && false }] == 0
  checkthat [expr { false && true }] == 0
  checkthat [expr { false && false }] == 0
  checkthat [expr { true && true }] == 1
  checkthat [expr { on || off }] == 1

  checkthat [! true] == 0
  checkthat [! false] == 1
  checkthat [! [! [! true]]] == 0
}

test "truth values" {
    checkthat [expr { bool(5) }] == 1
    checkthat [expr { bool(0.5) }] == 1
    checkthat [expr { bool(0.0) }] == 0
    checkthat [expr { bool("on") }] == 1
    checkthat [expr { bool("off") }] == 0
    checkthat [expr { bool("TRUE") }] == 1
}

test "ternary if" {
    checkthat [expr { 1 ? "yes" : "no" }] eq "yes"
    checkthat [expr { 0 ? "yes" : "no" }] eq "no"
    
    set things [list cat mouse bird dog lizard]

    checkthat [expr { "bird" in $things ? "there" : "not there"}] eq "there"
    checkthat [expr { "fish" in $things ? "there" : "not there"}] eq "not there"
}

test "list expr" {
    checkthat [expr [list 1 + 1]] == 2
}

test "hex math" {
    checkthat [expr 0xb + 0xb] == 22
    checkthat [expr 1 + 0xA] == 11
}

test "hex parse weird" {
    set x 0xff
    checkthat "[expr $x + $x]" eq {510}
    checkthat "[expr { $x + $x }]" eq {510}
}

test "exponential notation" {
    set foo 1e3
    checkthat [expr { $foo + 2 }] == 1002.0
    checkthat [expr { 1e3 + 2 }] == 1002.0
    checkthat [expr 1e3 + 2 ] == 1002.0
}
