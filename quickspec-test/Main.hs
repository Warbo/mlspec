{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Common
import           Control.Exception (try, SomeException)
import           Data.Either
import           Data.List
import           Data.List.Utils
import           Language.Eval
import           MLSpec.Theory
import           System.Directory
import           System.Exit
import           System.IO.Temp
import           System.Process
import qualified Test.Arbitrary.Cabal   as Cabal
import           Test.QuickCheck.Monadic
import           Test.Tasty             (defaultMain, testGroup, localOption)
import           Test.Tasty.QuickCheck

main = go $ testGroup "Tests depending on QuickSpec" [
    testProperty "No missing types"            noMissingTypes
  ]
  where go = defaultMain . localOption (QuickCheckTests 1)

noMissingTypes = monadicIO $ do
  Just out <- runBools
  mDebug out
  assert (noMissingTypeMessages out)
  where noMissingTypeMessages = not . (msg `isInfixOf`)
        msg = "WARNING: there are no variables of the following types"

-- Build a theory of Booleans and run it via Cabal
runBools = run doRun
  where doRun = eval thy
        thy       = withPkgs ["containers"] (asList syms)
        syms      = [
            (qualified "Data.Bool" "True",  Ty "Bool", A 0)
          , (qualified "Data.Bool" "False", Ty "Bool", A 0)
          , (qualified "Data.Bool" "not",   Ty "Bool -> Bool", A 1)
          , (qualified "Data.Bool" "||",    Ty "Bool -> Bool -> Bool", A 2)
          , (qualified "Data.Bool" "&&",    Ty "Bool -> Bool -> Bool", A 2)
          ]

qs = withPkgs ["quickspec"] . qualified "Test.QuickSpec"

[f0, f1, f2, f3, f4, f5] = map (qs . ("fun" ++)) ["0", "1", "2", "3", "4", "5"]
