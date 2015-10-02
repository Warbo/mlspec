{-# LANGUAGE TupleSections #-}
module Main where

import           Common
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
import           Language.Eval
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

main = defaultMain $ testGroup "All tests" [pureTests, impureTests]

pureTests = localOption (QuickCheckTests 10) $ testGroup "Pure tests" [
    testProperty "Theory gets packages"    canReadTheoryPkgs
  , testProperty "Theory gets modules"     canReadTheoryMods
  , testProperty "Theory gets names"       canReadTheoryNames
  , testProperty "Type modules included"   typeModulesIncluded
  , testProperty "Names are rendered"      renderedDefinitionContainsNames
  , testProperty "Names are monomorphised" renderedNamesAreMonomorphised
  , testProperty "Arities are rendered"    renderedDefinitionSetsArity
  , testProperty "Clusters give names"     renderedClusterContainsNames
  , testProperty "Modules import"          moduleImports
  , testProperty "Modules are Main"        modulesAreMain
  , testProperty "Modules run QuickSpec"   moduleRunsQuickSpec
  , testProperty "JSON <-> Entry"          canHandleJSONEntries
  , testProperty "JSON <-> Cluster"        canHandleJSONClusters
  , testProperty "Can read JSON clusters"  canReadJSONClusters
  , testProperty "Can read real JSON"      canReadRealJSON
  , testProperty "Can parse type sigs"     canParseTypeSigs
  , testProperty "Can extract type's mods" canExtractTypeMods
  , testProperty "Can limit theory size"   canLimitTheorySize
  ]

impureTests = localOption (QuickCheckTests 10) $ testGroup "Impure tests" [
    --testProperty "Project dirs made from clusters" projectsMadeFromClusters
  ]

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
        qualify (Mod m) (N n) = N (m ++ "." ++ n)

typeModulesIncluded :: [Pkg]
                    -> [Mod]
                    -> [Name]
                    -> [QType]
                    -> [Arity]
                    ->  Bool
typeModulesIncluded ps' ms' ns' ts' as' = equiv m (nub allMods)
  where T p m s = addTypeMods (theory (C entries))
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

renderedNamesAreMonomorphised :: [Symbol] -> Property
renderedNamesAreMonomorphised ss = conjoin $ map (test . mono) ss
  where test m = counterexample (show m ++ " `isInfixOf` " ++ show output)
                                (m `isInfixOf` output)
        mono s = "Helper.mono (" ++ quote s ++ ")"
        quote (Mod m, N n, _, _) = "'" ++ m ++ "." ++ n
        output = renderDef ss

renderedModuleUsesTemplateHaskell :: Theory -> Bool
renderedModuleUsesTemplateHaskell t =
  "TemplateHaskell" `elem` getExts (renderModule Nothing t)

getExts x = map strip                         .
              concatMap (split . stripL)      .
              filter ("LANGUAGE" `isInfixOf`) .
              takePragmas $ x
  where takePragmas ('{':'-':'#':s) = let (p, s') = pragmaFrom s
                                       in p : takePragmas s'
        takePragmas (c:s)           = takePragmas s
        takePragmas ""              = []
        pragmaFrom ('#':'-':'}':s)  = ("", s)
        pragmaFrom (c:s)            = let (p,   s') = pragmaFrom s
                                       in (c:p, s')
        pragmaFrom ""               = ("", "")
        split (',':s)               = let s' = split s
                                       in "":s'
        split (c:s)                 = case split s of
                                           []     -> [[c]]
                                           (o:os) -> (c:o):os
        split ""                    = []
        strip                       = dropWhile isSpace     .
                                          reverse           .
                                          dropWhile isSpace .
                                          reverse
        stripL ('L':'A':'N':'G':'U':'A':'G':'E':s) = stripL s
        stripL (c:s)                               = c : stripL s
        stripL []                                  = []

renderedClusterContainsNames c = all (`isInfixOf` output) allowed
  where output     = renderModule Nothing (T ps ms ss)
        T ps ms ss = theory line
        line       = C (map mkEntryUC c)
        allowed    = map (\(_, n, a, t) -> show n) ss

moduleImports t@(T _ ms _) =
  all ((`elem` lines output) . ("import qualified " ++) . show) ms
  where output = renderModule Nothing t

modulesAreMain t = "module Main where" `elem` lines output
  where output = renderModule Nothing t

moduleRunsQuickSpec t =
  "main = Test.QuickSpec.quickSpec (Helper.addVars theory)" `elem` lines output
  where output = renderModule Nothing t

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

canLimitTheorySize :: Theory -> Int -> Bool
canLimitTheorySize t n =
  length (renderModule (Just n) t) <= length (renderModule (Just (n+1)) t)

-- Helpers

mkEntry p m n t a = E (p, m, n, t, a)

-- Uncurried
mkEntryUC (p, m, n, t, a) = mkEntry p m n t a

mkEntries :: [Cluster] -> B.ByteString
mkEntries = encode

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

renderQType (QT (ms, t)) = t

newtype QType = QT ([Mod], Type) deriving (Show)

instance Arbitrary QType where
  arbitrary = do
    n   <- typeName
    Mod m <- arbitrary
    return (QT ([Mod m], Ty (m ++ "." ++ n)))

instance Arbitrary Entry where
  arbitrary = E <$> arbitrary
