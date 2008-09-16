namespace eval Stack {variable n 0}


proc Stack::Stack {} {
    variable n
    set instance [namespace current]::[incr n]
    namespace eval $instance {variable s {}}
    proc $instance { method args } "
        Stack::do $instance \$method \$args 
    "
    set instance
}

proc Stack::do { self method args } {
    variable [join [list $self ::s] ""]
    switch -- $method {
        push {eval lappend s $args}
        pop {
            if ![llength $s] { error "stack underflow" }
            K [lindex $s end] [set s [lrange $s 0 end-1]]
        }
        destroy {
            namespace delete $self
        }
        default { error "unknown method $method" }
    }
}

proc K {a b} {set a}


set s [Stack::Stack]

puts [$s push Hello]
puts [$s push World]
puts [$s pop]
puts [$s pop]
$s destroy
puts [$s push Hello]
