#!/bin/bash

cabal install && ghc test/Main.hs && test/Main
