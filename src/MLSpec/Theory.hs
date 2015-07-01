module MLSpec.Theory where

import           Data.List
import qualified Test.Arbitrary.Cabal   as Cabal
import           Test.Arbitrary.Haskell

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
  show t@(T pkgs mods syms) = show (pkgs, renderModule t)

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

mkCabal :: Theory -> Cabal.Project
mkCabal (T pkgs mods symbols) = Cabal.P {
    Cabal.name = "mlspec-temp"
  , Cabal.version = [1]
  , Cabal.headers = Cabal.S () []
  , Cabal.sections = [
      Cabal.S "executable Main" [
          ("build-depends", intercalate ", " (map show pkgs))
        , ("main-is", "Main.hs")
        ]
    ]
  , Cabal.files = [
    (([], "Main.hs"), H (renderModule (T pkgs mods symbols)))
    ]
  }

renderModule :: Theory -> String
renderModule (T pkgs mods symbols) = unlines [
    "module Main where"
  , renderImports mods
  , renderDef symbols
  ]

renderImports :: [Module] -> String
renderImports = unlines . map (("import qualified " ++) . show)

renderDef :: [Symbol] -> String
renderDef symbols = concat [
    "theory = ["
  , intercalate ", " (map theoryLine symbols)
  , "]"
  ]

theoriesFromClusters :: String -> [Theory]
theoriesFromClusters = map theory . lines
