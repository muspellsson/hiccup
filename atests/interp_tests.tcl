
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

test "interp code" {
    interp create candyxr
    checkthat [catch {candyxr eval { return 11 }}] == 0
    checkthat [catch {candyxr eval { set x 11 }}] == 0
    checkthat [catch { candyxr eval { break } }] == 3
    checkthat [catch { candyxr eval { continue } }] == 4
    assert_err { candyxr eval { error "Eep" } }
    finalize { interp candyxr }
}

proc is_hidden { int pr } {
    expr {$pr in [interp hidden $int]}
}

test "interp hidden" {
    interp create -safe foo
    checkthat [is_hidden foo "exit"] == 1
    checkthat [is_hidden foo "incr"] == 0

    checkthat [interp hidden] eq {}

    checkthat [foo hidden] eq [interp hidden foo]
    finalize { interp foo }
}

test "interp hide" {
    interp create foo
    checkthat [is_hidden foo "incr"] == 0
    assert_noerr { foo eval { set x 4; incr x } }
    interp hide foo incr
    checkthat [is_hidden foo "incr"] == 1
    assert_err { foo eval { set x 4; incr x } }
    finalize { interp foo }
}

test "interp hide w/ ::" {
    interp create foo
    foo eval { namespace eval yo { proc nop {} {} } }
    assert_err { interp hide foo yo::nop }
    finalize { interp foo }
}
