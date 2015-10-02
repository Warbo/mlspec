{-# LANGUAGE OverloadedStrings #-}
module MLSpec.Theory where

import           Control.Monad
import           Data.Aeson
import           Data.Char
import           Data.Data
import           Data.Generics.Aliases
import           Data.Hashable
import           Data.List
import           Data.Maybe
import           Data.Stringable
import           Data.Typeable
import           Language.Eval
import qualified Language.Haskell.Exts.Parser as HEP
import qualified Language.Haskell.Exts.Syntax as HES
import           System.IO.Unsafe
import qualified Test.Arbitrary.Cabal         as Cabal
import           Test.Arbitrary.Haskell

newtype Name    = N  String deriving (Eq, Ord)
newtype Type    = Ty String deriving (Eq, Ord)
newtype Arity   = A  Int    deriving (Eq, Ord)

unPkg  (Pkg  x) = x
unMod  (Mod  x) = x
unName (N  x) = x
unType (Ty x) = x

newtype Entry = E (Pkg, Mod, Name, Type, Arity) deriving (Show, Eq)

instance FromJSON Entry where
  parseJSON (Object o) = do
    p <- o .: "package"
    m <- o .: "module"
    n <- o .: "name"
    t <- o .: "type"
    a <- o .: "arity"
    return $ E (Pkg p, Mod m, N n, Ty t, A a)
  parseJSON _ = mzero

instance ToJSON Entry where
  toJSON (E (Pkg p, Mod m, N n, Ty t, A a)) = object [
      "package" .= p
    , "module"  .= m
    , "name"    .= n
    , "type"    .= t
    , "arity"   .= a
    ]

newtype Cluster = C [Entry] deriving (Eq)

instance FromJSON Cluster where
  parseJSON = fmap C . parseJSON

instance ToJSON Cluster where
  toJSON (C xs) = toJSON xs

type Symbol = (Mod, Name, Type, Arity)

instance Show Name where
  show (N  x) = x

instance Show Type where
  show (Ty x) = x

instance Show Arity where
  show (A  x) = show x

data Theory = T [Pkg] [Mod] [Symbol]

instance Show Theory where
  show t@(T pkgs mods syms) = show (pkgs, renderModule Nothing t)

getPkg :: Entry -> Pkg
getPkg (E (p, _, _, _, _)) = p

getMod :: Entry -> Mod
getMod (E (_, m, _, _, _)) = m

getName :: Entry -> Name
getName (E (_, _, n, _, _)) = n

getType :: Entry -> Type
getType (E (_, _, _, t, _)) = t

getArity :: Entry -> Arity
getArity (E (_, _, _, _, a)) = a

readMods :: Type -> [Mod]
readMods x = concatMap readMods'' (typeBits x)

typeBits (Ty t) = case HEP.parseType t of
  HEP.ParseFailed _ _ -> []
  HEP.ParseOk   x     -> [x]

readMods'' :: Data a => a -> [Mod]
readMods'' x = readMods' x ++ concat (gmapQ readMods'' x)

readMods' :: Data a => a -> [Mod]
readMods' = concat . gmapQ (const [] `extQ` typeMod)

typeMod (HES.ModuleName m) = [Mod m]

theoryLine :: Symbol -> String
theoryLine (  _,   _, _, A a) | a > 5 = ""  -- QuickSpec only goes up to fun5
theoryLine (Mod m, N n, _,   a)         = concat [
    "let f = $(Helper.mono ('", wrappedName, ")) ",
    "in \"", qname, "\" `Test.QuickSpec.fun", show a, "` f"
  ]
  where qname = m ++ "." ++ n
        wrappedName = if op n then "(" ++ qname ++ ")"
                              else qname
        op          = any isSym
        isSym c     = or [c `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String),
                          isPunctuation c,
                          isSymbol c]

theory :: Cluster -> Theory
theory (C es) = T (nub pkgs) (nub mods) (nub symbols)
  where pkgs     = map getPkg  es
        mods     = map getMod      es
        symbols  = [(m, n, t, a) | (E (_, m, n, t, a)) <- es]

addTypeMods :: Theory -> Theory
addTypeMods (T ps ms ss) = T ps (nub (ms ++ tms)) ss
  where tms = concat [readMods t | (_, _, t, _) <- ss]

addScope = withPkgs requiredDeps . withMods requiredMods

requiredDeps :: [Pkg]
requiredDeps = map Pkg ["MLSpec", "quickspec", "QuickCheck"]

requiredMods :: [Mod]
requiredMods = map Mod ["MLSpec.Helper", "Test.QuickSpec"]

renderModule :: Maybe Int -> Theory -> String
renderModule n (T pkgs mods symbols) = unlines [
  renderDef (case n of
               Nothing -> symbols
               Just m  -> take m symbols)
  ]

renderMain :: String -> String
renderMain x = "main = Test.QuickSpec.quickSpec (Helper.addVars (" ++ x ++ "))"

asList :: [Expr] -> Expr
asList []     = "[]"
asList (x:xs) = ("(:)" $$ x) $$ asList xs

renderDef :: [Symbol] -> String
renderDef symbols = concat [
    "theory = Test.QuickSpec.signature ["
  , "Test.QuickSpec.vars [\"i1\", \"i2\", \"i3\"] (undefined :: Integer)\n  , "
  , "Test.QuickSpec.vars [\"s1\", \"s2\", \"s3\"] (undefined :: String)\n  , "
  , intercalate "\n  , " (map theoryLine symbols)
  , "]"
  ]

theoriesFromClusters :: [Cluster] -> [Theory]
theoriesFromClusters = map theory

getProjects n s = let theories = theoriesFromClusters (readClusters s)
                   in theories

readClusters :: String -> [Cluster]
readClusters x = fromMaybe [] (decode . fromString $ x)
