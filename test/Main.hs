{-# LANGUAGE TupleSections, OverloadedStrings #-}
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
  , testProperty "Names are rendered"      renderedDefinitionContainsNames
  , testProperty "Names are monomorphised" renderedNamesAreMonomorphised
  , testProperty "Arities are rendered"    renderedDefinitionSetsArity
  , testProperty "Clusters give names"     renderedClusterContainsNames
  , testProperty "Modules run QuickSpec"   moduleRunsQuickSpec
  , testProperty "JSON <-> Entry"          canHandleJSONEntries
  , testProperty "JSON <-> Cluster"        canHandleJSONClusters
  , testProperty "Can read JSON clusters"  canReadJSONClusters
  , testProperty "Can read real JSON"      canReadRealJSON
  , testProperty "Can parse type sigs"     canParseTypeSigs
  , testProperty "Can extract type's mods" canExtractTypeMods
  ]

impureTests = localOption (QuickCheckTests 10) $ testGroup "Impure tests" [
    --testProperty "Project dirs made from clusters" projectsMadeFromClusters
  ]

-- Tests

canReadTheoryPkgs ps ms ns ts as = p == nub (take count ps)
  where T s     = theory (C entries)
        p       = concatMap getPkg s
        entries = zipWith5 mkEntry ps ms ns ts as
        count   = length entries

canReadTheoryMods ps ms ns ts as = m == nub (take count ms)
  where T s     = theory (C entries)
        m       = concatMap getMod s
        entries = zipWith5 mkEntry ps ms ns ts as
        count   = length entries

canReadTheoryNames ps ms ns ts as =
  counterexample (show (lhs, rhs)) (lhs == rhs)
  where lhs     = asStrs (map getExpr s)
        rhs     = asStrs (take count names)
        asStrs  = map exprOf
        T s     = theory (C entries)
        entries = zipWith5 mkEntry ps ms ns ts as
        count   = length entries
        names   = map raw ns

renderedDefinitionContainsNames :: [Entry] -> Bool
renderedDefinitionContainsNames es =
  all (`isInfixOf` output) (map (show . getExpr) es)
  where Expr (_, _, output) = renderDef es

renderedDefinitionSetsArity :: [Entry] -> Bool
renderedDefinitionSetsArity ss = all arityExists (map getArity ss)
  where Expr (_, _, output) = renderDef ss
        arityExists a = ("Test.QuickSpec.fun" ++ show a) `isInfixOf` output

renderedClusterContainsNames :: Theory -> Bool
renderedClusterContainsNames (T es) = all inExpr (map getExpr es)
  where inExpr (Expr (_, _, e)) = e `isInfixOf` output
        Expr (_, _, output) = renderDef es

renderedNamesAreMonomorphised :: [Entry] -> Property
renderedNamesAreMonomorphised ss = conjoin $ map (test . mono . getExpr) ss
  where test (Expr (_, _, m)) = counterexample (show m ++ " `isInfixOf` " ++ show output)
                                               (m `isInfixOf` output)
        mono s = qualified "Helper" "mono" $$ thQuote (wrapOp s)
        Expr (_, _, output) = renderDef ss

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


moduleRunsQuickSpec t =
  "main = Test.QuickSpec.quickSpec (Helper.addVars" `isPrefixOf` renderMain t

canHandleJSONEntries :: Entry -> Property
canHandleJSONEntries entry =
  not (null (getPkg entry)) &&
  not (null (getMod entry)) ==>
    case decode (encode entry) of
         Nothing -> error $ "Couldn't decode '" ++ S.toString (encode entry) ++ "'"
         Just x  -> x == entry

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

-- Helpers

mkEntry p m n t a = E (withPkgs [p] $ withMods [m] $ raw n, t, a)

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
    return $ C (zipWith5 (\p m n t a -> E (withPkgs [p] $ qualified m n, t, a)) ps ms ns ts as)

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
