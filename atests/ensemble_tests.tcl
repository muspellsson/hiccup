test "basic ensemble" {
    namespace eval foo {
        proc weezle { x } {
            return [expr {$x + 5}]
        }
        proc baz v { ::incr v }
        proc bonk x { expr { $x + 1 } }
        namespace export weezle baz
        namespace ensemble create

    }

    checkthat [foo baz 3] == 4
    checkthat [foo weezle 5] == 10
    assert_err { foo bonk 1 }

    finalize { ns foo }
}

test "ensemble exists" {
    checkthat [namespace ensemble exists string] == 1
    checkthat [namespace ensemble exists strings] == 0
    namespace eval foo {
        proc baz v { ::incr v }
        namespace export baz
        namespace ensemble create
    }
    checkthat [namespace ensemble exists foo] == 1
    checkthat [namespace ensemble exists puts] == 0
    checkthat [expr { "foo" in [info commands] }] == 1
    finalize { ns foo }
}

test "existing ensembles" {
    checkthat [namespace ensemble exists string]
    checkthat [namespace ensemble exists info]
}

test "ensemble removal" {
    testlib::ignore
    namespace eval foo {
        proc baz v { ::incr v }
        namespace export baz
        namespace ensemble create
    }
    assert_noerr { foo baz 4 }
    checkthat [namespace ensemble exists foo] == 1
    finalize { ns foo }
    assert_err { foo baz 4 }
    checkthat [namespace ensemble exists foo] == 0
}
