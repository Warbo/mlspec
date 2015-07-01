{-# LANGUAGE TupleSections #-}
module Main where

import           Control.Exception (try, SomeException)
import           Data.Either
import           Data.List
import           Data.String.Utils
import           MLSpec.Theory
import           System.Directory
import           System.Exit
import           System.Process
import           System.IO.Temp
import qualified Test.Arbitrary.Cabal   as Cabal
import           Test.Arbitrary.Haskell
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty             (defaultMain, testGroup, localOption)
import           Test.Tasty.QuickCheck

-- Only run Cabal tests if we have Cabal available; likewise for tests depending
-- on particular modules.
main = do cabal <- haveCabal
          deps  <- testsWithDeps
          let tests = if cabal then allTests (cabalTests:deps)
                               else allTests             deps
          defaultMain tests

allTests rest = testGroup "All tests" (pureTests:impureTests:rest)

pureTests = localOption (QuickCheckTests 10) $ testGroup "Pure tests" [
    testProperty "Can read packages"      canReadPackage
  , testProperty "Can read modules"       canReadPackage
  , testProperty "Can read names"         canReadName
  , testProperty "Theory gets packages"   canReadTheoryPkgs
  , testProperty "Theory gets modules"    canReadTheoryMods
  , testProperty "Theory gets names"      canReadTheoryNames
  , testProperty "Names are rendered"     renderedDefinitionContainsNames
  , testProperty "Arities are rendered"   renderedDefinitionSetsArity
  , testProperty "Clusters give names"    renderedClusterContainsNames
  , testProperty "Imports rendered"       renderedImports
  , testProperty "Modules import"         moduleImports
  , testProperty "Modules are Main"       modulesAreMain
  , testProperty "Modules run QuickSpec"  moduleRunsQuickSpec
  , testProperty "Package has Main.hs"    projectGetsModule
  , testProperty "Package has executable" projectHasExecutable
  , testProperty "Executable runs Main"   executableRunsMain
  , testProperty "Executable has deps"    executableHasDependencies
  ]

impureTests = localOption (QuickCheckTests 10) $ testGroup "Impure tests" [
    testProperty "Project dirs made from clusters" projectsMadeFromClusters
  ]

-- Tests which depend on the "cabal" command
cabalTests = localOption (QuickCheckTests 1) $ testGroup "Cabal tests" [
  ]

-- Tests which depend on the availability of particular modules
dependentTests = [
    mkDepTests ["Test.QuickSpec"] [
       testProperty "Cabal projects check OK"      cabalCheck
      , testProperty "Cabal projects configure OK" cabalConfigure
      , testProperty "Cabal projects run OK"       cabalRun
      ]
  ]
  where qs = [M "Test.QuickSpec"]
        mkDepTests ms ts = (map M ms, limit (testGroup (mkStr ms) ts))
        mkStr ms = "Tests depending on " ++ (intercalate " " ms)
        limit    = localOption (QuickCheckTests 1)

-- Tests

canReadPackage p m n = getPackage (mkEntry p m n) == p

canReadMod p m n = getMod (mkEntry p m n) == m

canReadName p (M m) (N n) = getName (mkEntry p (M m) (N n)) == N (m ++ "." ++ n)

canReadTheoryPkgs ps ms ns = p == nub (take count ps)
  where T p m s = theory (unwords entries)
        entries = zipWith3 mkEntry ps ms ns
        count   = length entries

canReadTheoryMods ps ms ns = m == nub (take count ms)
  where T p m s = theory (unwords entries)
        entries = zipWith3 mkEntry ps ms ns
        count   = length entries

canReadTheoryNames ps ms ns = map fst s == take count names
  where T p m s = theory (unwords entries)
        entries = zipWith3 mkEntry ps ms ns
        count   = length entries
        names   = zipWith qualify ms ns
        qualify (M m) (N n) = N (m ++ "." ++ n)

renderedDefinitionContainsNames ss =
  all (`isInfixOf` output) (map (show . fst) ss)
  where output = renderDef ss

renderedDefinitionSetsArity ss = all arityExists (map snd ss)
  where output        = renderDef ss
        exists        = (`isInfixOf` output)
        arityExists a = exists ("`Test.QuickSpec.fun" ++ show a ++ "`")

renderedClusterContainsNames (C c) =
  all (`isInfixOf` output) (map (show . fst) ss)
  where output     = renderModule (T ps ms ss)
        T ps ms ss = theory line
        line       = unwords (map mkEntryUC c)

renderedImports ms =
  all ((`elem` lines output) . ("import qualified " ++) . show) ms
  where output = renderImports ms

moduleImports t@(T _ ms _) =
  all ((`elem` lines output) . ("import qualified " ++) . show) ms
  where output = renderModule t

modulesAreMain t = "module Main where" `elem` lines output
  where output = renderModule t

moduleRunsQuickSpec t =
  "main = Test.QuickSpec.quickSpec theory" `elem` lines output
  where output = renderModule t

projectGetsModule t = mainMod == renderModule t
  where project          = mkCabal t
        Just (H mainMod) = lookup ([], "Main.hs") (Cabal.files project)

projectHasExecutable t = heading == "executable Main"
  where [Cabal.S heading _] = Cabal.sections project
        project             = mkCabal t

executableRunsMain t = main == "Main.hs"
  where Just main        = lookup "main-is" exec
        [Cabal.S _ exec] = Cabal.sections project
        project          = mkCabal t

executableHasDependencies t@(T ps _ _) = all (`elem` deps) (map show ps)
  where deps             = map strip $ split "," depline
        Just depline     = lookup "build-depends" exec
        [Cabal.S _ exec] = Cabal.sections project
        project          = mkCabal t

projectsMadeFromClusters cs = monadicIO $ do
  made <- run $ withSystemTempDirectory "mlspectest" getMade
  mDebug (("names", names), ("made", made))
  assert (all id made)
  where getMade dir = do writeTheoriesFromClusters dir clusters
                         mapM (existsIn dir) names
        clusters      = mkEntries cs
        theories      = map theory (lines clusters)
        names         = map (Cabal.name . mkCabal) theories
        existsIn x y  = doesDirectoryExist (x ++ "/" ++ y)

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
  where doConfig dir = cabalIn dir ["configure"]

cabalRun = monadicIO $ do
  (code, out, err) <- run $ withCabalProject thy doRun
  mDebug (code, out, err)
  assert (code == ExitSuccess)
  where doRun dir = cabalIn dir ["configure"] >> cabalIn dir ["run"]
        thy       = (T [P "containers"] [M "Data.Bool"] syms)
        syms      = [
            (N "Data.Bool.True",  0)
          , (N "Data.Bool.False", 0)
          , (N "Data.Bool.not",   1)
          , (N "Data.Bool.(||)",  2)
          , (N "Data.Bool.(&&)",  2)
          ]

withCabalProject :: Theory -> (FilePath -> IO a) -> IO a
withCabalProject t f = withSystemTempDirectory "mlspectest" go
  where go dir  = Cabal.makeProject dir project >>= f
        project = mkCabal t

-- Helpers

testsWithDeps = withDeps dependentTests
  where withDeps []             = return []
        withDeps ((ms, ts):tss) = do have <- haveDeps ms
                                     rest <- withDeps tss
                                     return $ if have then (ts:rest)
                                                      else rest

cabal args dir = cabalInWith dir args ""

cabalIn dir args = cabal args (Just dir)

cabalInWith dir args stdin = readCreateProcessWithExitCode cmd stdin
  where cmd = (proc "cabal" args) { cwd = dir }

haveCabal :: IO Bool
haveCabal = fmap isRight (run (cabal ["--help"] Nothing))
  where run  :: IO a -> IO (Either SomeException a)
        run   = try

haveDep :: Module -> IO Bool
haveDep (M m) = haveCabal >>= tryDep
  where tryDep False = return False
        tryDep _     = do
          (_, out, _) <- cabalInWith Nothing ["repl"] ("import " ++ m)
          -- Hacky, but GHCi still gives ExitSuccess :(
          return ("Could not find module" `isInfixOf` out)

haveDeps :: [Module] -> IO Bool
haveDeps ms = fmap (all id) (mapM haveDep ms)

debug :: Show a => a -> Property -> Property
debug  = whenFail . print

mDebug :: Show a => a -> PropertyM IO ()
mDebug = monitor  . debug

mkEntry (P p) (M m) (N n) = p ++ ":" ++ m ++ "." ++ n

-- Uncurried
mkEntryUC (p, m, n) = mkEntry p m n

mkEntries :: [Cluster] -> String
mkEntries cs = unlines $ map show cs

newtype Cluster = C [(Package, Module, Name)]

instance Show Cluster where
  show (C xs) = unwords $ map mkEntryUC xs

instance Arbitrary Cluster where
  arbitrary = do
    NonNegative n <- arbitrary
    ps <- vectorOf n arbitrary
    ms <- vectorOf n arbitrary
    ns <- vectorOf n arbitrary
    return $ C (zip3 ps ms ns)

mkCluster ps ms ns = (ps, ms, ns, zipWith3 mkEntry ps ms ns)

lower = "abcdefghijklmnopqrstuvwxyz"
upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

instance Arbitrary Module where
  arbitrary = do
    initials <- listOf1 (elements upper)
    rest     <- infiniteListOf (listOf (elements lower))
    return $ M $ intercalate "." (zipWith (:) initials rest)

instance Arbitrary Package where
  arbitrary = fmap P (listOf1 (elements lower))

instance Arbitrary Name where
  arbitrary = do
    initial <- elements lower
    rest    <- listOf (elements (lower ++ upper))
    return $ N (initial:rest)

instance Arbitrary Theory where
  arbitrary = do
    pkgs  <- arbitrary
    mods  <- arbitrary
    names <- arbitrary
    return $ T      pkgs
                    mods
               (zip names (repeat 0))
