module MLSpec.Theory where

import Data.Generics
import Data.List

newtype Package = P String deriving (Eq, Ord)
newtype Module  = M String deriving (Eq, Ord)
newtype Name    = N String deriving (Eq, Ord)

type Arity   = Int
type Symbol  = (Name, Arity)

instance Show Package where
  show (P x) = x

instance Show Module where
  show (M x) = x

instance Show Name where
  show (N x) = x

data Theory = T [Package] [Module] [Symbol]

instance Show Theory where
  show t@(T pkgs mods syms) = show (pkgs, renderTheory t)

getPackage :: String -> Package
getPackage = P . takeWhile (/= ':')

getMod :: String -> Module
getMod x = let N n = getName x
            in M . init . dropWhileEnd (/= '.') $ n

getName :: String -> Name
getName = N . tail . dropWhile (/= ':')

theoryLine :: Symbol -> String
theoryLine (N n, a) = "\"" ++ n ++ "\" `fun" ++ show a ++ "0` " ++ n

theory :: String -> Theory
theory l = T (nub pkgs) (nub mods) (nub symbols)
  where bits  = words l
        pkgs  = map getPackage bits
        names = map getName    bits
        mods  = map getMod     bits
        symbols = [(n, 0) | n <- names]

renderTheory (T pkgs mods symbols) = concat [
    "["
  , intercalate ", " (map theoryLine symbols)
  , "]"
  ]

theoriesFromClusters :: String -> [Theory]
theoriesFromClusters = map theory . lines
