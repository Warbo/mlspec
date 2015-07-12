{-# LANGUAGE TupleSections #-}
module Main where

import           Control.Exception (try, SomeException)
import qualified TestData
import           Data.Aeson
import qualified Data.ByteString.Lazy.Internal as B
import           Data.Char
import           Data.Either
import           Data.List
import           Data.Maybe
import           Data.String.Utils
import qualified Data.Stringable as S
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
    testProperty "Theory gets packages"    canReadTheoryPkgs
  , testProperty "Theory gets modules"     canReadTheoryMods
  , testProperty "Theory gets names"       canReadTheoryNames
  , testProperty "Type modules included"   typeModulesIncluded
  , testProperty "Names are rendered"      renderedDefinitionContainsNames
  , testProperty "Names are monomorphised" renderedNamesAreMonomorphised
  , testProperty "Arities are rendered"    renderedDefinitionSetsArity
  , testProperty "Clusters give names"     renderedClusterContainsNames
  , testProperty "Imports rendered"        renderedImports
  , testProperty "Modules import"          moduleImports
  , testProperty "Modules are Main"        modulesAreMain
  , testProperty "Modules run QuickSpec"   moduleRunsQuickSpec
  , testProperty "Package has Main.hs"     projectGetsModule
  , testProperty "Package has executable"  projectHasExecutable
  , testProperty "Executable runs Main"    executableRunsMain
  , testProperty "Executable has deps"     executableHasDependencies
  , testProperty "JSON <-> Entry"          canHandleJSONEntries
  , testProperty "JSON <-> Cluster"        canHandleJSONClusters
  , testProperty "Can read JSON clusters"  canReadJSONClusters
  , testProperty "Can read real JSON"      canReadRealJSON
  , testProperty "Can parse type sigs"     canParseTypeSigs
  , testProperty "Can extract type's mods" canExtractTypeMods
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

canReadTheoryPkgs ps ms ns ts as = p == nub (take count ps)
  where T p _ _ = theory (C entries)
        entries = zipWith5 mkEntry ps ms ns ts as
        count   = length entries

canReadTheoryMods ps ms ns ts as = m == nub (take count ms)
  where T p m s = theory (C entries)
        entries = zipWith5 mkEntry ps ms ns ts as
        count   = length entries

canReadTheoryNames ps ms ns ts as =
  map (\(m, n, _, _) -> qualify m n) s == take count names
  where T p m s = theory (C entries)
        entries = zipWith5 mkEntry ps ms ns ts as
        count   = length entries
        names   = zipWith qualify ms ns
        qualify (M m) (N n) = N (m ++ "." ++ n)

typeModulesIncluded :: [Package]
                    -> [Module]
                    -> [Name]
                    -> [QType]
                    -> [Arity]
                    ->  Bool
typeModulesIncluded ps' ms' ns' ts' as' = equiv m (nub allMods)
  where T p m s = theory (C entries)
        entries = zipWith5 mkEntry ps ms ns uts as
        uts     = map renderQType ts
        tms     = concatMap (\(QT (ms, _)) -> ms) ts
        allMods = ms ++ tms
        (ps, ms, ns, ts, as) = unzip5 (zip5 ps' ms' ns' ts' as')
        equiv xs ys = length xs == length ys &&
                          all (`elem` ys) xs &&
                          all (`elem` xs) ys

renderedDefinitionContainsNames :: [Symbol] -> Bool
renderedDefinitionContainsNames ss =
  all (`isInfixOf` output) (map (\(_, n, _, _) -> show n) ss)
  where output = renderDef ss

renderedDefinitionSetsArity :: [Symbol] -> Bool
renderedDefinitionSetsArity ss = all arityExists (map (\(_, _, _, a) -> a) ss)
  where output        = renderDef ss
        exists        = (`isInfixOf` output)
        arityExists a = exists ("`Test.QuickSpec.fun" ++ show a ++ "`")

renderedNamesAreMonomorphised :: [Symbol] -> Bool
renderedNamesAreMonomorphised ss = all (`isInfixOf` output) (map mono ss)
  where mono s = "Test.QuickCheck.All.monomorphic (" ++ quote s ++ ")"
        quote (M m, N n, _, _) = "'" ++ m ++ "." ++ n
        output = renderDef ss

renderedModuleUsesTemplateHaskell :: Theory -> Bool
renderedModuleUsesTemplateHaskell t =
  "TemplateHaskell" `elem` getExts (renderModule t)

getExts x = map strip         .
              concatMap split .
              map stripL      .
              filter ("LANGUAGE" `isInfixOf`) .
              takePragmas $ x
  where takePragmas ('{':'-':'#':s) = let (p, s') = pragmaFrom s
                                       in p : takePragmas s'
        takePragmas (c:s)           = takePragmas s
        takePragmas ""              = []
        pragmaFrom ('#':'-':'}':s) = ("", s)
        pragmaFrom (c:s)           = let (p,   s') = pragmaFrom s
                                      in (c:p, s')
        pragmaFrom ""              = ("", "")
        stripL ('L':'A':'N':'G':'U':'A':'G':'E':s) = stripL s
        stripL (c:s) = c : stripL s
        stripL [] = []
        split (',':s) = let s' = split s
                         in "":s'
        split (c:s)   = case split s of
                             []     -> [c:[]]
                             (o:os) -> ((c:o):os)
        split "" = []
        strip    = dropWhile isSpace . reverse . dropWhile isSpace . reverse

renderedClusterContainsNames c = all (`isInfixOf` output) allowed
  where output     = renderModule (T ps ms ss)
        T ps ms ss = theory line
        line       = C (map mkEntryUC c)
        allowed    = map (\(_, n, a, t) -> show n) ss

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

canHandleJSONEntries :: Entry -> Bool
canHandleJSONEntries entry =
  case decode encoded of
       Nothing -> error $ "Couldn't decode '" ++ S.toString encoded ++ "'"
       Just x  -> x == entry
   where encoded = encode entry

canHandleJSONClusters es =
  case decode encoded of
       Nothing -> error $ "Couldn't decode '" ++ S.toString encoded ++ "'"
       Just x  -> x == cluster
  where encoded = encode cluster
        cluster = C es

canReadJSONClusters :: [Cluster] -> Bool
canReadJSONClusters cs' = readClusters encoded == cs
  where encoded = S.toString (encode cs)
        cs      = take 10 cs'

canReadRealJSON = monadicIO readJSON
  where readJSON = do
          str <- run $ readFile "test/data/data-stringmap.clusters.json"
          assert (readClusters str == TestData.clusters)

canParseTypeSigs t = length (typeBits t) == 1

canExtractTypeMods (QT (ms, t)) = readMods t == ms

projectsMadeFromClusters cs = monadicIO $ do
  (claimed, made) <- run $ withSystemTempDirectory "mlspectest" getMade
  mDebug (("names",    names),
          ("claimed",  claimed),
          ("made",     made),
          ("clusters", clusters),
          ("theories", theories))
  assert (all snd made)
  where getMade dir  = do out1 <- writeTheoriesFromClusters dir clusters
                          out2 <- mapM (\n -> fmap (n,) (existsIn dir n)) names
                          return (out1, out2)
        clusters     = S.toString (mkEntries cs)
        theories     = map theory (readClusters clusters)
        names        = map (Cabal.name . mkCabal) theories
        existsIn x y = doesDirectoryExist (x ++ "/" ++ y)

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
            (M "Data.Bool", N "True",  Ty "Bool",                 A 0)
          , (M "Data.Bool", N "False", Ty "Bool",                 A 0)
          , (M "Data.Bool", N "not",   Ty "Bool -> Bool",         A 1)
          , (M "Data.Bool", N "||",    Ty "Bool -> Bool -> Bool", A 2)
          , (M "Data.Bool", N "&&",    Ty "Bool -> Bool -> Bool", A 2)
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

mkEntry p m n t a = E (p, m, n, t, a)

-- Uncurried
mkEntryUC (p, m, n, t, a) = mkEntry p m n t a

mkEntries :: [Cluster] -> B.ByteString
mkEntries cs = encode cs

instance Show Cluster where
  show (C xs) = S.toString (encode xs)

instance Arbitrary Cluster where
  arbitrary = do
    NonNegative n <- arbitrary
    ps <- vectorOf n arbitrary
    ms <- vectorOf n arbitrary
    ns <- vectorOf n arbitrary
    ts <- vectorOf n arbitrary
    as <- vectorOf n arbitrary
    return $ C (zipWith5 (\p m n t a -> E (p, m, n, t, a)) ps ms ns ts as)

mkCluster ps ms ns ts = (ps, ms, ns, zipWith4 mkEntry ps ms ns ts)

wrap x    = "(" ++ x ++ ")"

renderQType (QT (ms, t)) = t

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

typeName = do initial <- elements upper
              rest    <- listOf (elements (lower ++ upper ++ " "))
              return (initial:rest)

sizedListOf gen 0 = return []
sizedListOf gen n = do
  points <- listOf (choose (0, n))
  count  <- arbitrary
  let points' = take (abs count `mod` n) points
      diffs   = diffsOf (sort points')
  mapM gen diffs

diffsOf = diffsOf' 0
  where diffsOf' n [] = []
        diffsOf' n (x:xs) = x - n : diffsOf' x xs

sizedTypeName 0 = typeName
sizedTypeName n = do
  chunks <- sizedListOf sizedTypeName (n - 1)
  head   <- typeName
  return $ case chunks of
                [] -> head
                _  -> intercalate " -> " (map wrap chunks)

newtype QType = QT ([Module], Type) deriving (Show)

instance Arbitrary QType where
  arbitrary = do
    n   <- typeName
    M m <- arbitrary
    return (QT ([M m], Ty (m ++ "." ++ n)))

instance Arbitrary Type where
  arbitrary = do
    size <- arbitrary
    name <- sizedTypeName (abs size `mod` 100)
    return (Ty name)

instance Arbitrary Arity where
  arbitrary = fmap (A . abs . (`mod` 6)) arbitrary

instance Arbitrary Entry where
  arbitrary = E <$> arbitrary

instance Arbitrary Theory where
  arbitrary = do
    pkgs    <- arbitrary
    mods    <- arbitrary
    names   <- arbitrary
    types   <- arbitrary
    arities <- arbitrary
    return $ T      pkgs
                    mods
               (zip4 mods names types arities)
