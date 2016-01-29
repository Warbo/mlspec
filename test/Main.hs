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
  , testProperty "Template Haskell quotes" thQuotes
  , testProperty "Template Haskell deps"   thDeps
  , testProperty "getProjects"             getProjectsTest
  , testProperty "addScope"                addScopeTest
  , testProperty "letIn"                   letInTest
  ]

impureTests = localOption (QuickCheckTests 10) $ testGroup "Impure tests" [
    --testProperty "Project dirs made from clusters" projectsMadeFromClusters
  ]

-- Tests

canReadTheoryPkgs ps' ms ns ts as = p == nub (take count ps)
  where T s     = theory (C entries)
        p       = concatMap getPkg s
        entries = zipWith5 mkEntry ps ms ns ts as
        count   = length entries
        ps      = nub ps'

canReadTheoryMods ps ms ns ts as = m == nub (take count ms)
  where T s     = theory (C entries)
        m       = concatMap getMod s
        entries = zipWith5 mkEntry ps ms ns ts as
        count   = length entries

canReadTheoryNames (Positive count') = do
    let count = count' `mod` 10
    ns <- vectorOf count arbitrary
    xs <- vectorOf count arbitrary
    let entries = zipWith (\n (p, m, t, a) -> mkEntry p m n t a) ns xs
        T s     = theory (C entries)
        lhs     = map (eExpr . getExpr) s
        rhs     = ns
    return $ counterexample (show (lhs, rhs))
                            (lhs == rhs)

renderedDefinitionContainsNames (Positive n) =
    forAll (vectorOf (n `mod` 10) arbitrary) go
  where go :: [Entry] -> Bool
        go es = let output = eExpr (renderDef es)
                 in all (`isInfixOf` output) (map (eExpr . getExpr) es)

renderedDefinitionSetsArity (Positive n) =
    forAll (vectorOf (n `mod` 10) arbitrary) go
  where go :: [Entry] -> Bool
        go ss = let output        = eExpr (renderDef ss)
                    arityExists a = ("Test.QuickSpec.fun" ++ show a) `isInfixOf` output
                 in all arityExists (map getArity ss)

renderedClusterContainsNames (Positive count) = do
    es <- vectorOf (count `mod` 10) arbitrary
    let output   = eExpr (renderDef es)
        inExpr e = eExpr e `isInfixOf` output
    return $ all inExpr (map getExpr es)

renderedNamesAreMonomorphised (Positive n) =
    forAll (vectorOf (n `mod` 10) arbitrary) go
  where go :: [Entry] -> Property
        go ss = let test x = let m = eExpr x
                              in counterexample (show m ++ " `isInfixOf` " ++ show output)
                                                (m `isInfixOf` output)
                    mono s = qualified "MLSpec.Helper" "mono" $$
                             thQuote (wrapOp s)
                    output = eExpr (renderDef ss)
                 in conjoin $ map (test . mono . getExpr) ss

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


moduleRunsQuickSpec t ts =
  "main = " `isPrefixOf` renderMain ts t

--canHandleJSONEntries :: Entry -> Property
canHandleJSONEntries m p e t a =
  let entry = E (withMods [m] $ withPkgs [p] $ raw e, t, a)
   in counterexample (show [Just entry, decode (encode entry)])
                     (case decode (encode entry) of
                           Nothing -> error $ "Couldn't decode '" ++ S.toString (encode entry) ++ "'"
                           Just x  -> x == entry)

canHandleJSONClusters es' =
  case decode encoded of
       Nothing -> error $ "Couldn't decode '" ++ S.toString encoded ++ "'"
       Just x  -> x == cluster
  where encoded = encode cluster
        cluster = C es
        es      = map single (filter empty es')
        empty x = not (null (getPkg x) || null (getMod x))
        single (E (e, t, a)) = E (e { ePkgs = take 1 (ePkgs e),
                                      eMods = take 1 (eMods e)},
                                  t,
                                  a)

canReadJSONClusters :: [Cluster] -> Bool
canReadJSONClusters cs' = readClusters encoded == cs
  where encoded = S.toString (encode cs)
        cs      = take 10 (prune cs')
        prune [] = []
        prune (C es:cs) = let es' = pruneE es
                           in if null es' then prune cs
                                          else C (take 5 es'):prune cs
        pruneE (E (Expr {
                      ePkgs=p:_,
                      eMods=m:_,
                      eExpr=e
                   }, t, a):es) = E (Expr {
                                        ePkgs=[p],
                                        eMods=[m],
                                        eExpr=e,
                                        eFlags=[],
                                        ePreamble=[]
                                     }, t, a) : pruneE es
        pruneE (e:es) = pruneE es
        pruneE [] = []

canReadRealJSON = once $ monadicIO readJSON
  where readJSON = do
          str <- run $ readFile "test/data/data-stringmap.clusters.json"
          mDebug (diff (readClusters str) TestData.clusters)
          assert (readClusters str == TestData.clusters)
        diff xs ys = (filter (`notElem` ys) xs, filter (`notElem` xs) ys)

canParseTypeSigs t = length (typeBits t) == 1

canExtractTypeMods (QT (ms, t)) = readMods t == ms

thQuotes :: Expr -> Property
thQuotes x =
    counterexample (show (eExpr x, e', q))
                   (if hasOp then wrapped else not wrapped)
  where hasOp   = any isSym e'
        wrapped = "'(" `isInfixOf` q
        q       = eExpr (thQuote (wrapOp (raw e')))
        e'      = filter (/= '.') e
        e       = eExpr x

thDeps :: Expr -> Bool
thDeps e = "-XTemplateHaskell" `elem` eFlags (thUnquote e) &&
           "-XTemplateHaskell" `elem` eFlags (thQuote   e)

runTheoryTest = runTheory

runTheoriesFromClustersTest = runTheoriesFromClusters

-- | We use `single` since our JSON encoding only allows one mod/pkg
getProjectsTest :: Cluster -> Property
getProjectsTest (C es) = not (null es) ==>
                         let c  = C (map single es)
                             j  = S.toString (encode (toJSON [c]))
                             ps = getProjects j
                             ts = [theory c]
                             single (E (e, t, a)) = E (single' e, t, a)
                             single' e = e {
                               eMods = take 1 (eMods e),
                               ePkgs = take 1 (ePkgs e)
                             }
                          in counterexample (show (j, ts, ps)) (ps == ts)

addScopeTest x = let x' = addScope x
                  in all (`elem` ePkgs x') requiredDeps &&
                     all (`elem` eMods x') requiredMods

letInTest = let e = eExpr (letIn [(raw "x", raw "1")] (raw "y"))
             in counterexample e (e == "let {(x) = (1)} in (y)")

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
