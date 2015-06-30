module Main where

import Data.List
import MLSpec.Theory
import Test.QuickCheck
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

main = defaultMain $ testGroup "All tests" [
    testProperty "Can read packages"    canReadPackage
  , testProperty "Can read modules"     canReadPackage
  , testProperty "Can read names"       canReadName
  , testProperty "Theory gets packages" canReadTheoryPkgs
  , testProperty "Theory gets modules"  canReadTheoryMods
  , testProperty "Theory gets names"    canReadTheoryNames
  ]

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

mkEntry (P p) (M m) (N n) = p ++ ":" ++ m ++ "." ++ n

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
