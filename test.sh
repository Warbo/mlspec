#!/usr/bin/env bash

cabal build exe:MLSpec &&
cabal test         && {
  rm -f *.tix
  cabal run mlspec-test-quickspec
}
