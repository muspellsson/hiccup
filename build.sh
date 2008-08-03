#!/bin/bash
EXTRA="-W"
if [[ $1 == "all" ]];
  then EXTRA="$EXTRA -no-recomp";
fi

if [[ $1 == "prof" ]]; 
  then ghc -O2 -fglasgow-exts -o hiccup Main.hs -funbox-strict-fields --make -prof -auto-all;
elif [[ $1 == "simp" ]];
 then ghc -O2 -fglasgow-exts Common.hs -funbox-strict-fields -funfolding-use-threshold=24 -ddump-simpl --make $EXTRA > common-simp.hs;
else
ghc -O2 -fvia-c -optc-O3 -fglasgow-exts -o hiccup Main.hs -funbox-strict-fields -funfolding-use-threshold=24 --make $EXTRA;
fi
