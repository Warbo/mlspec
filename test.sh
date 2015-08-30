#!/usr/bin/env bash

cabal test

rm -f *.tix
cabal run mlspec-test-quickspec
