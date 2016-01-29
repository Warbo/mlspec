{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Common
import           Control.Exception (try, SomeException)
import           Data.Aeson
import           Data.Either
import           Data.List
import           Data.List.Utils
import           Data.Maybe
import qualified Data.Stringable as S
import           Language.Eval
import           Language.Eval.Internal
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
    testProperty "No missing types"       noMissingTypes
  , testProperty "Variables are distinct" getDistinctArbitraries
  , testProperty "Get equations"          getEquations
  ]
  where go = defaultMain . localOption (QuickCheckTests 1)

noMissingTypes = monadicIO $ do
    Just out <- theoryGo bools
    monitor (counterexample out)
    assert (noMissingTypeMessages out)
  where noMissingTypeMessages = not . (msg `isInfixOf`)
        msg = "WARNING: there are no variables of the following types"

getDistinctArbitraries = monadicIO $ do
    Just out <- theoryGo ints
    monitor (counterexample out)
    assert (noEqualVars out)
  where noEqualVars = ("0 raw equations" `isInfixOf`)

getEquations  = monadicIO $ do
    Just out <- theoryGo bools
    monitor (counterexample out)
    gotJson out
  where json :: String -> [Maybe Value]
        json       = map (decode . S.fromString) . filter seemsJson  . lines
        gotJson xs = let j = json xs
                      in do assert (all isJust j)
                            assert (not (null  j))
        seemsJson ""      = False
        seemsJson ('D':_) = False
        seemsJson _       = True

-- Build and run theories
ints = [
    E (withPkgs ["containers"] "show",  Ty "Integer -> String", A 1)
  ]

theoryGo syms = do
    monitor (counterexample (unlines ("":(imports ++ [main]))))
    x <- run (runTheory (theory (C syms)))
    monitor (counterexample (fromMaybe "Got Nothing" x))
    return x
  where r       = renderDef syms
        imports = map mkImport (eMods r)
        main    = unlines (ePreamble r ++ [renderMain [] (eExpr r)])

bools = let f = withPkgs ["containers"] . qualified "Data.Bool"
         in [
    E (f "True",  Ty "Bool",                 A 0)
  , E (f "False", Ty "Bool",                 A 0)
  , E (f "not",   Ty "Bool -> Bool",         A 1)
  , E (f "||",    Ty "Bool -> Bool -> Bool", A 2)
  , E (f "&&",    Ty "Bool -> Bool -> Bool", A 2)
  ]

qs = withPkgs ["quickspec"] . qualified "Test.QuickSpec" . raw

[f0, f1, f2, f3, f4, f5] = map (qs . ("fun" ++)) ["0", "1", "2", "3", "4", "5"]
