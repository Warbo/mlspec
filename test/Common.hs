module Common where

import Data.List
import Language.Eval
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
    exprs   <- arbitrary
    types   <- arbitrary
    arities <- arbitrary
    return $ T (map E (zip4 exprs types arities (repeat (H False))))

instance Arbitrary Expr where
  arbitrary = do ps <- arbitrary
                 ms <- arbitrary
                 m  <- arbitrary
                 n  <- arbitrary
                 return $ withPkgs ps $ withMods ms $ qualified m (raw n)

instance Arbitrary Arity where
  arbitrary = fmap (A . abs . (`mod` 6) . (\(NonNegative x) -> x)) arbitrary

instance Arbitrary Type where
  arbitrary = do
    size <- arbitrary
    name <- sizedTypeName (abs size `mod` 100)
    return (Ty name)

instance Arbitrary Mod where
  arbitrary = do
    initials <- listOf1 (elements upper)
    rest     <- infiniteListOf (listOf (elements lower))
    return $ Mod $ intercalate "." (zipWith (:) initials rest)

instance Arbitrary Pkg where
  arbitrary = fmap Pkg (listOf1 (elements lower))
