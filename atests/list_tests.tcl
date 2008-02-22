
test "list test" {
  checkthat [list] eq {}

  set bean [list 1 2 3 4 5 {6 7 8}]

  checkthat [llength $bean] == 6

  checkthat [llength "peanut"] == 1
  checkthat [llength "peanut ontology"] == 2
  checkthat [llength ""] == 0

  checkthat [llength {one [puts bean]}] == 3

  checkthat [llength {a b # c d}] == 5

  checkthat [llength [list [list 1 2 3] [list 3 4 5]]] == 2

  set boo [list {} {} {} {}]
  checkthat [llength $boo] == 4
}

test "lindex" {
  checkthat [lindex 4] == 4
  checkthat [lindex 4 {}] == 4

  set bean [list 1 2 3 4 5 {6 7 8}]

  checkthat [lindex $bean 3] == 4
  checkthat [lindex $bean 5] eq {6 7 8}
  checkthat [lindex $bean 8] eq "" 

  checkthat [lindex $bean -3] eq {} 

  checkthat [lindex $bean end] eq {6 7 8}
}

test "lappend" {
  set x {}
  lappend x 1
  lappend x 2 3 "entropy kills"
  checkthat [llength $x] == 4
  checkthat $x eq "1 2 3 {entropy kills}"
}

test "lset no index" {
  checkthat [not [info exists boo]]
  assertErr  { lset boo 5 }
  set boo 2
  lset boo 5
  checkthat $boo == 5
  lset boo {} 6
  checkthat $boo == 6
}

test "lset single index" {
  set lst [list 1 2 3]
  lset lst 0 ONE
  checkthat $lst eq {ONE 2 3}
  
  assertErr { lset lst 40 TOO_HIGH }
  assertErr { lset lst -3 TOO_LOW }
}

test "lassign basic" {
  lassign { 1 2 3 } a b c
  checkthat $a == 1
  checkthat $b == 2
  checkthat $c == 3
}

test "lassign return" {
  checkthat [lassign {0 1 2 3} x] eq {1 2 3}
  checkthat [lassign {0 1 2} a b c] eq {}
}

test "lassign length difference" {
  lassign { 0 1 } a b c d
  checkthat $a == 0
  checkthat $b == 1
  checkthat $c eq ""
  checkthat $d eq ""
}

test "lsort basic" { 
  checkthat [lsort {}] eq {}
  checkthat [lsort {a b c}] eq {a b c}
  checkthat [lsort {c b a}] eq {a b c}
}

test "lsort decreasing" {
  checkthat [lsort -increasing {c b a}] eq {a b c}
  checkthat [lsort -decreasing {c b a}] eq {c b a}
  checkthat [lsort -decreasing {a b c}] eq {c b a}
}

test "lsort nocase" {
  set lst { A b a c B C }

  checkthat [lsort $lst] eq {A B C a b c}
  checkthat [lsort -nocase $lst] eq {A a b B c C}
}
