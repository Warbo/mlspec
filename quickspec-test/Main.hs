module Main where

import           Common
import           Control.Exception (try, SomeException)
import           Data.Either
import           Data.List
import           Data.List.Utils
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
    testProperty "Cabal projects check OK"     cabalCheck
  , testProperty "Cabal projects configure OK" cabalConfigure
  , testProperty "Cabal projects run OK"       cabalRun
  , testProperty "No missing types"            noMissingTypes
  ]
  where go = defaultMain . localOption (QuickCheckTests 1)

cabalCheck t = monadicIO $ do
  (code, out, err) <- run $ withCabalProject t doCheck
  mDebug (code, out, err)
  assert (code == ExitSuccess)
  where doCheck dir = cabalIn dir ["check"]

cabalConfigure (T _ ms ss) = monadicIO $ do
  (code, out, err) <- run $ withCabalProject (T [P "containers"] ms ss)
                                             doConfig
  mDebug (code, out, err)
  assert (code == ExitSuccess)

cabalRun = monadicIO $ do
  (code, out, err) <- runBools
  mDebug (code, out, err)
  assert (code == ExitSuccess)

noMissingTypes = monadicIO $ do
  (code, out, err) <- runBools
  mDebug (code, out, err)
  assert (noMissingTypeMessages out)
  where noMissingTypeMessages = not . (msg `isInfixOf`)
        msg = "WARNING: there are no variables of the following types"

mkShellNix d = do (code, out, err) <- cmdIn d "cabal2nix" ["--shell", "./."]
                  case code of
                    ExitSuccess -> return out
                    _           -> error ("mkShellNix '" ++ d ++ "': " ++ err)

doConfig dir = do sn <- mkShellNix dir
                  writeFile (dir ++ "/shell.nix") sn
                  cmdIn dir "nix-shell" ["--run", "cabal configure"]

-- Build a theory of Booleans and run it via Cabal
runBools = run $ withCabalProject thy doRun
  where doRun dir = do doConfig dir
                       cabalIn dir ["run"]
        thy       = T [P "containers"] [M "Data.Bool"] syms
        syms      = [
            (M "Data.Bool", N "True",  Ty "Bool",                 A 0)
          , (M "Data.Bool", N "False", Ty "Bool",                 A 0)
          , (M "Data.Bool", N "not",   Ty "Bool -> Bool",         A 1)
          , (M "Data.Bool", N "||",    Ty "Bool -> Bool -> Bool", A 2)
          , (M "Data.Bool", N "&&",    Ty "Bool -> Bool -> Bool", A 2)
          ]

withCabalProject :: Theory -> (FilePath -> IO a) -> IO a
withCabalProject t f = withSystemTempDirectory "mlspectest" go
  where go dir  = Cabal.makeProject dir project >>= f
        project = mkCabal Nothing t

cmdIn d c as = readCreateProcessWithExitCode (proc c as) { cwd = Just d } ""

cabal args dir = cabalInWith dir args ""

cabalIn dir args = cabal args (Just dir)

cabalInWith dir args = readCreateProcessWithExitCode cmd
  where cmd = (proc "cabal" args) { cwd = dir }
