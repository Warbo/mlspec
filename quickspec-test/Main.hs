{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Common
import           Control.Exception (try, SomeException)
import           Data.Either
import           Data.List
import           Data.List.Utils
import           Data.Maybe
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
    testProperty "No missing types"                noMissingTypes
  , testProperty "Variables are distinct"          getDistinctArbitraries
  ]
  where go = defaultMain . localOption (QuickCheckTests 1)

noMissingTypes = monadicIO $ do
    Just out <- theoryGo bools
    mDebug out
    assert (noMissingTypeMessages out)
  where noMissingTypeMessages = not . (msg `isInfixOf`)
        msg = "WARNING: there are no variables of the following types"

getDistinctArbitraries = monadicIO $ do
    Just out <- theoryGo ints
    mDebug out
    assert (noEqualVars out)
  where noEqualVars = ("0 raw equations" `isInfixOf`)

-- Build and run theories
ints = [
    E (withPkgs ["containers"] "show",  Ty "Integer -> String", A 1)
  ]

theoryGo syms = do let r       = renderDef syms
                       imports = map mkImport (eMods r)
                       main    = unlines (ePreamble r ++ [renderMain [] (eExpr r)])
                   run (putStrLn (unlines ("":(imports ++ [main]))))
                   run (do x <- runTheory (theory (C syms))
                           putStrLn (fromMaybe "Got Nothing" x)
                           return x)

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
