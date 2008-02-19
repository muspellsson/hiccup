
test "list test" {
  set bean [list 1 2 3 4 5 {6 7 8}]

  checkthat [llength $bean] == 6
  checkthat [lindex $bean 3] == 4
  checkthat [lindex $bean 5] eq {6 7 8}

  checkthat [llength "peanut"] == 1
  checkthat [llength "peanut ontology"] == 2
  checkthat [llength ""] == 0

  checkthat [llength {one [puts bean]}] == 3

  checkthat [llength {a b # c d}] == 5

  checkthat [llength [list [list 1 2 3] [list 3 4 5]]] == 2
  checkthat [lindex 4] == 4
  checkthat [lindex $bean 8] eq "" 

  set boo [list {} {} {} {}]
  checkthat [llength $boo] == 4
}


test "lappend" {
  set x {}
  lappend x 1
  lappend x 2 3 "entropy kills"
  checkthat [llength $x] == 4
  checkthat $x eq "1 2 3 {entropy kills}"
}
