{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Common
import           Control.Exception (try, SomeException)
import           Data.Either
import           Data.List
import           Data.List.Utils
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
    testProperty "No missing types"            noMissingTypes
  ]
  where go = defaultMain . localOption (QuickCheckTests 1)

noMissingTypes = monadicIO $ do
  Just out <- runBools
  mDebug out
  assert (noMissingTypeMessages out)
  where noMissingTypeMessages = not . (msg `isInfixOf`)
        msg = "WARNING: there are no variables of the following types"

-- Build a theory of Booleans and run it
runBools = do let r = renderDef syms
              run (print (unlines [mkImports  (eMods r),
                                   renderMain (eExpr r)]))
              run (do x <- runTheory (theory (C syms))
                      putStrLn (case x of
                                     Just s  -> s
                                     Nothing -> "Got Nothing")
                      return x)
  where syms = [
            E (f "True",  Ty "Bool",                 A 0)
          , E (f "False", Ty "Bool",                 A 0)
          , E (f "not",   Ty "Bool -> Bool",         A 1)
          , E (f "||",    Ty "Bool -> Bool -> Bool", A 2)
          , E (f "&&",    Ty "Bool -> Bool -> Bool", A 2)
          ]
        f = withPkgs ["containers"] . qualified "Data.Bool"

qs = withPkgs ["quickspec"] . qualified "Test.QuickSpec" . raw

[f0, f1, f2, f3, f4, f5] = map (qs . ("fun" ++)) ["0", "1", "2", "3", "4", "5"]
