#!/usr/bin/env bash

cabal build exe:MLSpec              &&
cabal test --show-details=streaming && {
  rm -f *.tix
  cabal run mlspec-test-quickspec
}
