module Common where

import Data.List
import MLSpec.Theory
import Test.QuickCheck
import Test.QuickCheck.Monadic

debug :: Show a => a -> Property -> Property
debug  = whenFail . print

mDebug :: Show a => a -> PropertyM IO ()
mDebug = monitor  . debug

sizedTypeName 0 = typeName
sizedTypeName n = do
  chunks <- sizedListOf sizedTypeName (n - 1)
  head   <- typeName
  return $ case chunks of
                [] -> head
                _  -> intercalate " -> " (map wrap chunks)

typeName = do initial <- elements upper
              rest    <- listOf (elements (lower ++ upper ++ " "))
              return (initial:rest)

wrap x    = "(" ++ x ++ ")"

lower = "abcdefghijklmnopqrstuvwxyz"
upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

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

instance Arbitrary Arity where
  arbitrary = fmap (A . abs . (`mod` 6)) arbitrary

instance Arbitrary Type where
  arbitrary = do
    size <- arbitrary
    name <- sizedTypeName (abs size `mod` 100)
    return (Ty name)

instance Arbitrary Name where
  arbitrary = do
    initial <- elements lower
    rest    <- listOf (elements (lower ++ upper))
    return $ N (initial:rest)

instance Arbitrary Module where
  arbitrary = do
    initials <- listOf1 (elements upper)
    rest     <- infiniteListOf (listOf (elements lower))
    return $ M $ intercalate "." (zipWith (:) initials rest)

instance Arbitrary Package where
  arbitrary = fmap P (listOf1 (elements lower))
