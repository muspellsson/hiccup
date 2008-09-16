 proc every {interval script} {
    global everyIds
    if {$interval eq "cancel"} {
       after cancel $everyIds($script)
       return
    }
    set everyIds($script) [after $interval [info level 0]]
    set rc [catch {uplevel #0 $script} result]
    if {$rc == [catch break]} {
        after cancel $everyIds($script)
        set rc 0
    } elseif {$rc == [catch continue]} {
        # Ignore - just consume the return code
        set rc 0
    }
    # TODO: Need better handling of errorInfo etc...
    return -code $rc $result
 }


 every 1000 {puts TICK}
 every 1500 {puts TOCK}
 vwait forever
