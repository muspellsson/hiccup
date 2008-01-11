die() {
  echo $1
  exit 1
}

ghc -e Tests.runUnit Tests.hs || die "build failure"
runghc Main.hs atests.tcl

