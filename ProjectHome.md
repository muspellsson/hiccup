### Intro ###
`hiccup` is an interpreter for a subset of tcl.

### New Features ###
  * `vwait` works
  * `format` and `interp` have basic functionality (`interp -safe` mostly works)
  * `expr` works as expected, is efficient, and is supported in conditionals. Also, ternary `if`s now work.
  * `apply` and `lmap` have been added
  * Floating point is supported natively (as in, not by constantly reparsing strings)
  * `{*}` works.
  * Arrays work
  * Namespaces are supported (including 'export', 'import', and 'forget')
  * `switch` is fully supported
  * non-naive list handling
  * Basic support for file channels has been added. Now files can be opened, closed, read from, written to, and appended.
  * Procs can now have optional arguments. The 'args' parameter is also supported.
  * Thanks to Haskell's laziness, hiccup does some pseudo-shimmering and memoizes parsing. As a result, things are about 30% faster.
  * The REPL now uses readline.
  * Error messages are much more consistent and useful, and it is significantly harder to create an error that will crash the interpreter.

### Background ###
`hiccup` was mostly inspired by [picol](http://antirez.com/page/picol). picol is a neat little tcl subset interpreter in 550 lines of C. I'd been looking for a language to implement in haskell, and picol made me wonder, "Can I make a haskell interpreter that is just as speedy or speedier with less lines?". I was pretty sure the answer would be "Yes", since haskell has the benefit of more libraries and abstractions, and it seems I was mostly right. `hiccup` had well under 300 lines of code, and I think (minus type declarations, whitespace, comments, tests, and includes :-P ) it still does, despite my compulsion to add random library functions here and there.

I didn't know tcl when I started, so it was also a lovely exercise in learning the language. As I went along, I discovered that the fundamentals of the language are pretty elegant, but most practical things involve messy details that I don't care enough to implement. I'd like to stress that _hiccup isn't a complete tcl interpeter, and it is not intended to be_.


If anyone has any suggestions for improvements that are within the scope of what I was trying to do here (some basic features, keep it small and relatively efficient), I'd be very interested to hear. I'm looking at you, haskell gurus.


_Note: The purpose of this thing wasn't to display my skill or advocate for haskell. It was a bit of fun in my idle time and an exploration of my interest in programming languages. Drawing any conclusions about haskell, tcl, me, or the nature of reality based on this would be silly. :)_

### Flaws ###
  * `interp` is incomplete.
  * Poor support for IO commands.. `fconfigure`, `socket`, `fileevent` not implemented yet.
  * I made this in small blocks of otherwise idle time. I'm not a haskell expert, I'm not a tcl expert, and I wasn't trying all that hard. :-P

### Features ###
  * I'm unaware of any inconsistency with real Tcl in parsing or basic operations.
  * Can easily be embedded in haskell programs and given custom commands.
  * supports upvar, uplevel, and global
  * Fairly complete namespace support
  * allows ${bracket variable names}
  * has puts, exit, eval, return, break, catch, continue, while, if, for, foreach, switch, source, append, split, time, srand, rand.. and more!
  * supports lists, allows 'args' binding in method calls
  * has some basic math stuff
  * Supports some basic string, list, and array operations
  * allows running with 'hiccup filename' or without argument as a repl
  * various other things
_Look at the stuff in the "atests" (acceptance tests) directory for more examples of functionality._

### Future ###
  * Soon
    * `namespace ensemble` support
    * ::tcl::string:: namespaces for ensembles, ::tcl::mathop, and similar

  * Probably
    * `dict` support
    * `chan` ensemble (including reflectedchan)
    * A way to extend it to allow user-created tcl types.

  * Maybe
    * Futures
    * `dict` support
    * A more sophisticated bytecode and compiler.

### Example ###

```

# Here is an example of some stuff hiccup can do.
# I think it's neat.

namespace import ::tcl::mathop::*

proc decr { v { i -1 } } {
  upvar $v loc
  incr loc $i
}

proc memfib x {
  set ::loc(0) 1
  set ::loc(1) 1
  for {set ctr 2} { $ctr <= $x } {incr ctr} {
    set v1 $::loc([- $ctr 1])
    set v2 $::loc([- $ctr 2])
    set {the sum} [+ $v1 $v2]
    set ::loc($ctr) ${the sum}
  }
  return $::loc($x)
}

set fcount 21
puts "First $fcount fibonacci numbers in descending order:"
while { 2 <= $fcount } {
  puts -nonewline "[memfib $fcount] "
  decr fcount
}
puts "\nDone."

proc say_i_havent_been_to { args } {
  foreach name $args {
    puts "I've never been to $name."
  }
}

say_i_havent_been_to Spain China Russia Argentina "North Dakota"

proc is v {
  return $v
}

foreach num {0 1 2 3 4 5 6 7 8 9} {
  set type [switch -- $num {
    1 - 9         {is odd}
    2 - 3 - 5 - 7 {is prime}
    0 - 4 - 6 - 8 {is even}
    default       {is unknown}
  }]
  puts "$num is $type"
}

puts [expr { sin(4) + 44.5 + rand()}]


```