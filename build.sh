#!/bin/bash
ghc -O2 -fvia-c -optc-O3 -fglasgow-exts -o hiccup Main.hs -funbox-strict-fields -funfolding-use-threshold=16 --make 
#ghc -O2 -fvia-c -fglasgow-exts -o hiccup Main.hs -funbox-strict-fields --make -prof -auto-all
