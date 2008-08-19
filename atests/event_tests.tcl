test "idle then update" {
    namespace eval foo {
        variable bar
    }
    after idle { set ::foo::bar 4 }
    update
    update
    checkthat $::foo::bar == 4
    finalize { ns foo }
}

test "after w/ delay" { 
    assert_noerr { after 500 }
    assert_err { after idle }
}

test "after info" {
    checkthat [llength [after info]] == 0
}


test "after cancel" {
    checkthat [llength [after info]] == 0
    set eid [after 10000 { puts BOO! }]
    checkthat [llength [after info]] == 1
    after cancel $eid
    checkthat [llength [after info]] == 0
}
