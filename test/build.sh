#!/bin/bash

p=$(pwd)

cobjs="../foreign-src/qhull_wrapper.o"

cd ../foreign-src
gcc -I/usr/include/qhull -DNDEBUG -std=c99 -O2 -Wall -c qhull_wrapper.c
#gcc -I/usr/include/qhull -std=c99 -O2 -Wall -c qhull_wrapper.c
cd $p

ghc -O2 -fforce-recomp -o bench --test Main.hs -i../src $cobjs -lpthread -lqhull

