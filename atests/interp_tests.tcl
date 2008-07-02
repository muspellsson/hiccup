
test "create, exists, delete" { 
  checkthat [interp create banana] eq banana
  checkthat [interp exists banana] == 1 {banana exists}
  interp delete banana
  checkthat [interp exists banana] == 0 {banana was deleted}
}

test "interp eval" {
  interp create foo
  interp eval foo {set foo_x 4}
  checkthat [info exists foo_x] == 0
  checkthat [interp eval foo {info exists foo_x}]
  checkthat [interp eval foo {incr foo_x}] == 5

  finalize { interp foo }
}

test "interp errors" {
    interp create foo
    assert_err { foo eval Do The Monkey Dance }
    assert_err { foo eval set a b c d }
    interp create { foo bar }
    assert_err { foo eval bar eval die }
    finalize { interp foo }
}

test "interp proc" {
  interp create pizz
  pizz eval {set pizz_x 4}
  checkthat [info exists pizz_x] == 0
  checkthat [pizz eval {info exists pizz_x}]
  checkthat [pizz eval {incr pizz_x}] == 5
  
  interp delete pizz

  checkthat [expr {"pizz" in [info commands]}] == 0
}

test "interp issafe" {
    checkthat [interp issafe] == 0
    assert_err { interp issafe pinochet }
}

test "safe interp create" {
    assert_noerr { interp create -safe xxx }
    checkthat [interp issafe xxx] == 1

    interp create dirty
    checkthat [interp issafe dirty] == 0

    finalize { interp xxx interp dirty }
}

test "safe really safe" {
    interp create -safe whee
    whee eval {set x 4}
    assert_err {whee eval {exit}}
    finalize {interp whee}
}

test "safe interp issafe" { 
    interp create -safe ii
    checkthat [interp issafe ii]
    checkthat [ii eval { interp issafe }]

    interp create uu
    checkthat [interp issafe uu] == 0
    checkthat [uu eval {interp issafe}] == 0
    finalize {interp ii interp uu}
}

test "interp create (no name)" {
    set n [interp create]
    interp eval $n {set x 4}
    checkthat [$n eval {info exists x}]
    interp delete $n
}

test "interp delete" {
    interp create a
    interp create {a b}
    checkthat [interp exists {a b}]
    assert_noerr { a eval b eval set x 4 }
    interp delete {a b}
    checkthat [interp exists {a b}] == 0
    checkthat [interp exists a]

    assert_err { a eval b eval { puts "OH NO" } }
    assert_noerr { a eval { set x 4 } }
    finalize { interp a }
}

test "interp path 1" {
    interp create a
    interp create {a b}
    checkthat [interp exists a] == 1 {normal 'create a' works}
    checkthat [interp exists {a b}] == 1 { {a b} exists}
    checkthat [interp exists b] == 0

    checkthat [interp eval {a b} {expr { 3 + 3 }}] == 6
    checkthat [a eval b eval set x 4] == 4
    finalize {interp a}
}

test "interp slaves" {
    checkthat [interp slaves] eq {}
    interp create foo
    interp create bar
    checkthat [lsort [interp slaves]] eq [list bar foo]
    finalize {interp foo interp bar}
}
