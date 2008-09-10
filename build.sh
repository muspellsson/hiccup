#!/bin/bash
EXTRA="-W"
if [[ $1 == "all" ]];
  then EXTRA="$EXTRA -no-recomp";
fi

if [[ $1 == "prof" ]]; 
  then ghc -O2 -fglasgow-exts -o hiccup Main.hs -fvia-c -funbox-strict-fields -prof -auto-all --make;
elif [[ $1 == "simp" ]];
 then ghc -O2 -fglasgow-exts Main.hs -funbox-strict-fields -ddump-simpl --make $EXTRA > all-simp.hs;
elif [[ $1 == "fast" ]]; 
  then ghc -O0 -fglasgow-exts -o hiccup Main.hs -funbox-strict-fields --make -no-recomp;
elif  [[ $1 == "all" ]];
  then ghc -O2 -fglasgow-exts -o hiccup Main.hs -funbox-strict-fields --make $EXTRA;
elif  [[ $1 == "" ]];
  then ghc -O2 -fglasgow-exts -o hiccup Main.hs -funbox-strict-fields --make $EXTRA;
else
    echo "Invalid argument: $1";
    exit -1;
fi
