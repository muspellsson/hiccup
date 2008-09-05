#!/bin/bash
EXTRA="-W"
if [[ $1 == "all" ]];
  then EXTRA="$EXTRA -no-recomp";
fi

if [[ $1 == "prof" ]]; 
  then ghc -O2 -fglasgow-exts -o hiccup Main.hs -funbox-strict-fields --make -prof -auto-all -v;
elif [[ $1 == "simp" ]];
 then ghc -O2 -fglasgow-exts TclObj.hs -funbox-strict-fields -funfolding-use-threshold=24 -ddump-simpl --make $EXTRA > tobj-simp.hs;
elif [[ $1 == "fast" ]]; 
  then ghc -O0 -fglasgow-exts -o hiccup Main.hs -funbox-strict-fields --make -no-recomp;
elif  [[ $1 == "all" ]];
  then ghc -O2 -fvia-c -optc-O3 -fglasgow-exts -o hiccup Main.hs -funbox-strict-fields -funfolding-use-threshold=24 --make $EXTRA;
elif  [[ $1 == "" ]];
  then ghc -O2 -fvia-c -optc-O3 -fglasgow-exts -o hiccup Main.hs -funbox-strict-fields -funfolding-use-threshold=24 --make $EXTRA;
else
    echo "Invalid argument: $1";
    exit -1;
fi
