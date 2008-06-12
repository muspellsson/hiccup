die() {
  echo $1
  exit 1
}

ghc -e Tests.runUnit Tests.hs || die "build failure"
runghc -W Main.hs atests.tcl

