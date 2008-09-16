proc scheckwords {wl} {
 set words [split [read [open "/usr/share/dict/words"]] "\n"]
 foreach w $wl {
   puts "$w is[expr {$w in $words ? " " : " not "}]a word"
 }
}

#puts [::tcl::unsupported::disassemble pro scheckwords]

scheckwords $argv
