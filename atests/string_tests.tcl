
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
