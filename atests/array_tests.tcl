
test "array set/get" {
  set boo(4) 111
  checkthat "$boo(4)" == 111
  set boo(5) 112
  checkthat $boo(5) == 112

  set boo(wallaby) "next tuesday"
  checkthat $boo(wallaby) eq "next tuesday"

  set "boo( oh no! )" 4
  checkthat "$boo( oh no! )" == 4

  proc succ {v} { return [+ $v 1] }
  set i 0
  set arr($i) 0
  set "arr([succ [succ $i]])" 1
  checkthat "$arr([succ [succ $i]])" == 1

  finalize { proc succ }
}


test "array size" {
  checkthat [array size boo] == 0
  set boo(0) 1
  set boo(1) 5
  set boo(2) 2

  checkthat [array size boo] == 3
}

test "array names" {
  set arr(0) 1
  set arr(feed) people
  checkthat [llength [array names arr]] == 2
}

test "array names pat" {
  set arr(0) 1
  set arr(feed) people
  checkthat [llength [array names arr oot]] == 0
  checkthat [llength [array names arr ?]] == 1
  checkthat [llength [array names arr ??]] == 0
  checkthat [llength [array names arr f*]] == 1
  checkthat [llength [array names arr f??d]] == 1
}

test "array names mode pat" {
  set arr(0) 1
  set arr(feed) people
  set arr(food) chickens
  assertErr { array names arr -bad 4 }
  checkthat [llength [array names arr -glob ?]] == 1
  checkthat [llength [array names arr -exact ?]] == 0
  checkthat [llength [array names arr -glob ??]] == 0
  checkthat [llength [array names arr -glob f*]] == 2
  checkthat [llength [array names arr -exact food]] == 1
  checkthat [llength [array names arr -glob f??d]] == 2
  checkthat [llength [array names arr -exact f??d]] == 0
}

test "array exists" {
  checkthat [array exists arr1] == 0

  set notarr 44
  checkthat [array exists notarr] == 0

  set arr1(0) 1

  checkthat [array exists arr1] == 1
}

test "array vs scalar" {
  assertErr {
    set x 4
    set x(1) 4
  }
}

test "array set" {
  assertErr { 
    array set arr { x 1 y 2 z }
  }

  array set arr {
    1 one
    2 two
    3 three
    4 four
  }

  checkthat $arr(1) eq one
  checkthat $arr(2) eq two
  checkthat $arr(3) eq three
  checkthat $arr(4) eq four

  assertErr { checkthat $arr(5) eq five }

  array set arr {
    5 five
    3 threee
  }

  checkthat $arr(5) eq five
  checkthat $arr(3) eq threee
}

test "array get" {
  array set arr { x "1 2" y {2 3} "handy man" 3 }
  checkthat [llength [array get arr]] == 6
  checkthat [llength [array get donkey]] == 0
}

test "array get pat" {
  array set arr { x "1 2" y {2 3} "handy man" 3 }
  checkthat [llength [array get arr]] == 6
  checkthat [llength [array get arr x]] == 2
  checkthat [llength [array get arr ?]] == 4
}

test "array exists with ind" {
  set x(4) 4
  set x(5) 5
  checkthat [array exists x]
  checkthat [not [array exists x(4)]]
}

test "array reset no-no" {
  set x(1) 44
  checkthat $x(1) == 44
  assertErr { set x 2 }
}

test "array unset full" {
  set x(4) 4
  set x(5) 5
  checkthat [array exists x]
  array unset x
  checkthat [not [array exists x]]
}

test "array unset patt" {
  set x(cat) 1
  set x(dog) 2
  set x(car) 2

  array unset x b*
  checkthat [array size x] == 3

  array unset x ca?
  checkthat [array size x] == 1
}
