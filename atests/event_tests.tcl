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
