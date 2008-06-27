
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
    finalize { interp xxx }
}

test "interp create (no name)" {
    set n [interp create]
    interp eval $n {set x 4}
    checkthat [$n eval {info exists x}]
    interp delete $n
}
