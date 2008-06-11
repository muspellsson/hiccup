
test "string methods" {
  checkthat 4 == [string length "five"]
  checkthat 0 == [string length ""]
  checkthat 7 == [string length "one\ntwo"]
  checkthat 4 == [string length "h\n\ti"]

  set fst [string index "whee" 1]
  checkthat "h" eq $fst

  checkthat "wombat" eq [string tolower "WOMBAT"]
  checkthat "CALCULUS" eq [string toupper "calculus"]
  checkthat "hello" eq [string trim "  hello  "]


  checkthat [string reverse "123"] eq "321"
  checkthat [string reverse ""] eq ""
  checkthat [string reverse "X Y"] eq "Y X"
}

test "string index" {
  set fst [string index "whee" 1]
  checkthat $fst eq "h"

  checkthat [string index "what" end] eq "t"

  checkthat [string index "" end] eq ""
  checkthat [string index "hi" 10] eq ""

  checkthat [string index "ABCDE" end-2] eq "C"
  checkthat [string index "ABCDE" end-0] eq "E"
}

test "string match" {
  checkthat [string match aa aa]
  checkthat [string match aa ab] == 0
  checkthat [string match "WOW" "wow"] == 0
  checkthat [string match -nocase "WOW" "wow"]

  checkthat [string match "a*e" "awesome"] == 1
  checkthat [string match "?arry" "Larry"] == 1
  checkthat [string match "?arry" "Larr?"] == 0
  checkthat [string match "L??ry" "Leary"] == 1
}

test "string compare" {
  checkthat [string compare "hi" "hi"] == 0
  checkthat [string compare "h" "hi"] == -1 
  checkthat [string compare "hi" "h"] == 1 
}

test "string compare -nocase" {
  checkthat [string compare "hi" "HI"] != 0 {hi /= HI without -nocase}
  checkthat [string compare -nocase "hi" "HI"] == 0
  checkthat [string compare -nocase "hi9" "HI"] != 0
  checkthat [string compare -nocase -nocase] == 0
  checkthat [string compare -nocase -nocase -nocase] == 0
  checkthat [string compare -nocase -nocase -nocase -nocase] == 0
  checkthat [get_err {string compare -nocase duck duck goose}] eq {bad option "duck": must be -nocase or -length}
}

test "string compare -length" {
  checkthat [string compare -length 4 "abcdYES" "abcdNO"] == 0
  checkthat [get_err {string compare -length button x x}] eq {expected integer but got "button"}

  checkthat [string compare -length -10 boo boo] == 0
  checkthat [string compare -length -1 bot boo] == 1
  checkthat [string compare -length -1 boo bot] == -1
}

test "string range" {
  checkthat [string range "ABCD" 1 2] eq "BC"
  checkthat [string range "ABCD" 1 11] eq "BCD"
  checkthat [string range "ABCD" -1 11] eq "ABCD"
  checkthat [string range "ABCD" 0 0] eq "A"
  checkthat [string range "ABCD" 1 0] eq ""
  checkthat [string range "ABCD" end end] eq "D"
  checkthat [string range "ABCD" end-1 end] eq "CD"
}

test "completion" {
  assert_err { string l "one" }
  checkthat [string le "one"] == 3
}

test "string equal" {
    checkthat [string equal "boo" "boo"]
    checkthat [string equal "boo" "foo"] == 0

    checkthat [string equal -nocase "boo" "BOO"]
    checkthat [string equal -length 2 XXa XXb]
}
