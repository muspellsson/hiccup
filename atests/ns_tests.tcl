test "namespace current" {
  checkthat [namespace current] == "::"
  namespace eval boo {
    checkthat [namespace current] eq "::boo"
  }
  finalize { ns boo }
}

test "namespace parent" {
  checkthat [namespace parent] eq {}
  namespace eval boo {
    checkthat [namespace parent] eq "::"
  }

  checkthat [namespace parent boo] eq "::"
  finalize { ns boo }
}

test "namespace children" {

  namespace eval boo55 {
    proc nothing {} {}
    checkthat [namespace children] == {}
  }

  checkthat [lsearch [namespace children] ::boo55] != -1
  finalize { ns boo55 }
  checkthat [lsearch [namespace children] ::boo55] == -1
}

test "namespace children ?ns?" {
  namespace eval foo::boo55 {
    proc nothing {} {}
  }

  checkthat [lsearch [namespace children foo] ::foo::boo55] != -1
  finalize { ns foo::boo55 }
  checkthat [lsearch [namespace children foo] ::foo::boo55] == -1
}

test "namespace proc 1" {
  finalize { namespace temp } {
    namespace eval temp {
      proc one {} { return 1 }
      checkthat [namespace current] == "::temp"
      checkthat [namespace parent] == "::"
      checkthat [one] == 1
    }

    assert_err { one }

    assert_noerr { ::+ 4 5 }

    checkthat [temp::one] == 1
    checkthat [::temp::one] == 1
  }
}


test "nested ns" {
  namespace eval abc {
    namespace eval xyz {
      proc tada {} { return "yay!" }
    }
  }

  assertErr { tada }
  assertErr { abc::tada }
  assertErr { ::abc::tada }
  assertErr { ::xyz::tada }
  assertErr { xyz::tada }

  checkthat [abc::xyz::tada] eq "yay!"
  checkthat [::abc::xyz::tada] eq "yay!"
}

test "double ns" {
  finalize { ns abc2 } {
    namespace eval abc2::xyz2 {
        proc tada {} { return "yay!" }
    }
    checkthat [abc2::xyz2::tada] eq "yay!"
  }
}

test "simple variable" {
  finalize { namespace foo } {
    namespace eval foo {
      variable wow 99
    }
   checkthat $foo::wow == 99
   checkthat $::foo::wow == 99
   set ::foo::wow 3
   checkthat $foo::wow == 3 
 }
}


test "namespace varible array syntax" {
  finalize { ns temp } {
    assertErr {
      namespace eval temp {
        variable boo(1)
      }
    }
  }
}

test "namespace variable evil" {
  finalize { proc evil ns temp_ns } {
    proc evil {} {
      checkthat $::temp_ns::value == 4
    }

    namespace eval temp_ns {
      variable value 4
      ::evil
    }
  }
}

test "namespace delete" {
  namespace eval foo {
    proc something {} { return 1 }
  }
  checkthat [namespace exists foo]
  namespace delete foo
  checkthat [not [namespace exists foo]]
}

test "ns variable scalar" {
  namespace eval hidden { variable IDS 4 }

  proc getit {} {
    variable ::hidden::IDS
    return $IDS
  }

 proc setit {} {
   variable ::hidden::IDS
   set IDS 11
 }

  checkthat [getit] == 4
  setit
  checkthat [getit] == 11

  finalize { proc setit proc getit namespace hidden }
}

test "ns variable array" {
  namespace eval foo {
    variable farr
    array set farr {
      1 one
      2 two
      3 three
    }

    checkthat $farr(1) eq one
  } 

  proc get_index ind {
    variable ::foo::farr 
    return $farr($ind)
  }
  
  checkthat [get_index 2] eq two

  finalize { ns foo proc get_index }
}

test "ns variable undefined scalar" {
  namespace eval hidden { variable IDS }

  proc getit {} {
    variable ::hidden::IDS
    return $IDS
  }

 proc setit {} {
   variable ::hidden::IDS
   set IDS 11
 }

  assertErr { getit }
  setit
  checkthat [getit] == 11

  finalize { proc setit proc getit namespace hidden }
}

test "ns proc variable" {
  namespace eval foo {
    variable boo 10
    proc doit {} {
      checkthat [not [info exists boo]]
      variable boo
      checkthat $boo == 10
      checkthat [info exists boo]
    }
    doit
  }

  finalize { namespace foo }
}

test "ns variable array 'array size'" {
  namespace eval foo {
    variable arr
    array set arr { 1 one 2 two 3 three }
  }

  checkthat [array size ::foo::arr] == 3
  finalize { namespace foo }
}

test "namespace tail" {
  checkthat [namespace tail boo] == boo
  checkthat [namespace tail ::oh::no] == no
  checkthat [namespace tail oh::no] == no
  checkthat [namespace tail ::] == {}
  checkthat [namespace tail {}] == {}
}

test "namespace qualifiers" {
  checkthat [namespace qualifiers boo] == {}
  checkthat [namespace qualifiers ::oh::no] == ::oh
  checkthat [namespace qualifiers oh::no] == oh
  checkthat [namespace qualifiers ::] == {}
}

test "namespace import/export simple" {
  namespace eval foo { 
    proc bat {} { return 1 }
    proc bar {} { return 2 }
    proc baz {} { return 3 }
    proc buz {} { return 4 }
    namespace export bar baz buz
  }

  namespace eval goo {
    assertErr { bar } 
    assertErr { baz } 
    assertErr { buz } 
    namespace import ::foo::ba?
    checkthat [bar] == 2
    checkthat [baz] == 3
    assertErr { bat }
    assertErr { buz }
  }
}

test "namespace origin, global" {
  proc weezer {} { return "W" }
  checkthat [namespace origin weezer] eq {::weezer}
  finalize { proc weezer }
}

test "namespace origin, ns" {
  namespace eval boo { proc eep {} { return 0 } }
  checkthat [namespace origin ::boo::eep] eq {::boo::eep}
  finalize { ns boo }
}

proc proc_exists pn { 
  expr { $pn in [info procs] }
}

test "namespace origin after import" {
  namespace eval boo { namespace export eep; proc eep {} { return OK } }
  checkthat [namespace origin ::boo::eep] eq {::boo::eep}
  checkthat [::boo::eep] eq OK

  namespace import ::boo::*
  checkthat [eep] eq OK
  checkthat [namespace origin eep] eq {::boo::eep}

  finalize { ns boo }
}

test "imported procs go away when parent is deleted" {
  namespace eval boo { namespace export fancy; proc fancy {} { return OK } }
  checkthat [namespace origin ::boo::fancy] eq {::boo::fancy}
  namespace import ::boo::*

  checkthat [::boo::fancy] eq OK
  checkthat [fancy] eq OK

  checkthat [namespace origin fancy] eq {::boo::fancy}

  namespace delete ::boo

  checkthat [namespace exists boo] == 0
  checkthat [proc_exists fancy] == 0
}

test "delete imported" {
  namespace eval boo { namespace export blah; proc blah {} { return OK }  }
  namespace import ::boo::blah

  checkthat [blah] eq OK
  rename blah {}
  checkthat [proc_exists blah] == 0
  checkthat [::boo::blah] eq OK

  finalize { ns boo }
}

test "ns export pattern" {
  namespace eval boo {
    namespace export g* e*
    proc golly {} { return OK }
    proc eep {} { return OK }
    proc faulty {} { return OK }
  }

  namespace import -force ::boo::*

  checkthat [golly] eq OK
  checkthat [eep] eq OK
  assert_err { faulty }

  finalize { namespace boo }
}

test "ns export -clear" {
  namespace eval boo {
    proc aaa {} { return OK }
    proc bbb {} { return OK }
    namespace export aaa
    namespace export -clear bbb
  }

  namespace eval baz {
    namespace import ::boo::*
    checkthat [bbb] eq OK
    assertErr { aaa }
  }

  finalize { namespace boo namespace baz }
}

test "ns export no args returns pats" {
  namespace eval boo {
    namespace export a b? c*
    checkthat [namespace export] eq [list a b? c*]
  }
}

test "ns import overwrite fails" {
  namespace eval boo {
    namespace export b*
    proc baz {} { return OK }
  }

  proc baz {} { whatever }

  assert_err { namespace import boo::baz }

  finalize { namespace boo proc baz }
}

test "default to global" {
  namespace eval foo {
    proc ret5 {} { return OK }
  }

  namespace eval other {
    foo::ret5
    assert_noerr { foo::ret5 }
  }
}

test "default ns path" {
  checkthat [namespace path] eq {}
  namespace eval foo {
    checkthat [namespace path] eq {}
  }
}

test "simple forget unqualified" {
   namespace eval baz {
      namespace export blah
      proc blah {} { return OK }
   }

   checkthat [proc_exists blah] == 0
   namespace import ::baz::blah
   checkthat [proc_exists blah] == 1

   namespace forget blah
   checkthat [proc_exists blah] == 0

   finalize { ns baz }
}

test "simple forget unqualified (no import)" {
   proc blah {} { return OK }

   checkthat [proc_exists blah] == 1

   namespace forget blah
   checkthat [proc_exists blah] == 1

   finalize { proc blah }
}

test "qualified forget" {
   namespace eval baz2 {
      namespace export blah2
      proc blah2 {} { return OK }
   }

   checkthat [proc_exists blah2] == 0
   namespace import ::baz2::blah2
   checkthat [proc_exists blah2] == 1

   namespace forget ::baz2::b*

   checkthat [proc_exists blah2] == 0

   finalize { ns baz2 }
}

test "info commands has all accessable" {
    namespace eval panther {
        proc baz {} {}
        checkthat [expr { "baz" in [info commands] }]
        checkthat [expr { "puts" in [info commands] }]
    }
    finalize { ns panther }
}
