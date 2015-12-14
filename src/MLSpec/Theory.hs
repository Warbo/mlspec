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
import           Language.Eval.Internal
import qualified Language.Haskell.Exts.Parser as HEP
import qualified Language.Haskell.Exts.Syntax as HES
import           System.IO.Unsafe
import qualified Test.Arbitrary.Cabal         as Cabal
import           Test.Arbitrary.Haskell

newtype Type  = Ty String deriving (Eq, Ord)
newtype Arity = A  Int    deriving (Eq, Ord)

unPkg  (Pkg  x) = x
unMod  (Mod  x) = x
unFlag (Flag x) = x
unType (Ty   x) = x

newtype Entry = E (Expr, Type, Arity) deriving (Show, Eq)

instance FromJSON Entry where
  parseJSON (Object o) = do
    p <- o .: "package"
    m <- o .: "module"
    n <- o .: "name"
    t <- o .: "type"
    a <- o .: "arity"
    return $ E (withPkgs [Pkg p] $ withMods [Mod m] $ raw n, Ty t, A a)
  parseJSON _ = mzero

instance ToJSON Entry where
  toJSON (E (e, Ty t, A a)) = object [
      "package" .= unPkg (head (ePkgs e))
    , "module"  .= unMod (head (eMods e))
    , "name"    .= eExpr e
    , "type"    .= t
    , "arity"   .= a
    ]

newtype Cluster = C [Entry] deriving (Eq)

instance FromJSON Cluster where
  parseJSON = fmap C . parseJSON

instance ToJSON Cluster where
  toJSON (C xs) = toJSON xs

instance Show Type where
  show (Ty x) = x

instance Show Arity where
  show (A  x) = show x

data Theory = T [Entry] deriving (Show)

instance Eq Theory where
  T xs == T ys = all (`elem` ys) xs && all (`elem` xs) ys

getExpr :: Entry -> Expr
getExpr (E (e, _, _)) = e

getPkg :: Entry -> [Pkg]
getPkg = ePkgs . getExpr

getMod :: Entry -> [Mod]
getMod = eMods . getExpr

getName :: Entry -> String
getName = eExpr . getExpr

getType :: Entry -> Type
getType (E (_, t, _)) = t

getArity :: Entry -> Arity
getArity (E (_, _, a)) = a

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

theoryLine :: Entry -> [Expr]
theoryLine (E (_, _, A a)) | a > 5 = []  -- QuickSpec only goes up to fun5
theoryLine (E (e, _,   a))         = [letIn [(name, val)] x]
  where name = "f"
        val  = thUnquote (mono $$ thQuote (wrapOp e))
        x    = (func $$ quoted (raw (eExpr e))) $$ name
        func = qualified "Test.QuickSpec" (wrapped "" (show a) "fun")
        mono = withPkgs ["mlspec-helper"] $ qualified "MLSpec.Helper" "mono"

-- | Wraps operators in parentheses, eg. "(<>)", leaves alphanumeric names alone
wrapOp :: Expr -> Expr
wrapOp x = if any isSym (eExpr x) then parens x
                                  else x

isSym '.'  = False -- To avoid module qualification
isSym '\'' = False -- "Prime" mark
isSym '_'  = False -- Counts as a letter
isSym c    = c `elem` ("!#$%&*+/<=>?@\\^|-~:" :: String) ||
               isPunctuation c                           ||
               isSymbol      c

-- | Add a preceding quote "'" to an Expr. Should be used with wrapOp.
thQuote :: Expr -> Expr
thQuote x = withFlags ["-XTemplateHaskell"] $ x { eExpr = "'" ++ exp }
  where exp = if '.' `elem` eExpr x
                 then eExpr x
                 else mod ++ eExpr x
        mod = case eMods x of
                   (Mod m:_) -> m ++ "."
                   []        -> ""

wrapped :: String -> String -> Expr -> Expr
wrapped o c x = x { eExpr = o ++ eExpr x ++ c }

parens :: Expr -> Expr
parens = wrapped "(" ")"

thUnquote :: Expr -> Expr
thUnquote = withFlags ["-XTemplateHaskell"] . wrapped "$(" ")"

quoted :: Expr -> Expr
quoted = wrapped "\"" "\""

-- | Let expressions. We use Expr on the left-hand-side, to allow
--   pattern-matching on constructors from arbitrary packages.
letIn :: [(Expr, Expr)] -> Expr -> Expr
letIn []                 x   = x
letIn nvs e = Expr {
      ePkgs  = nub ps'
    , eMods  = nub ms'
    , eFlags = nub fs'
    , eExpr  = expr
    }
  where ps'  = ePkgs  e ++ concatMap ePkgs  nves
        ms'  = eMods  e ++ concatMap eMods  nves
        fs'  = eFlags e ++ concatMap eFlags nves
        x    = eExpr e
        nves = map fst nvs ++ map snd nvs
        expr = "let {" ++ intercalate ";" defs ++ "} in (" ++ x ++ ")"
        defs = map mkDef nvs
        mkDef (n, v) = "(" ++ eExpr n ++ ") = (" ++ eExpr v ++ ")"

theory :: Cluster -> Theory
theory (C es) = T (nub es)

addScope = withPkgs requiredDeps . withMods requiredMods

requiredDeps :: [Pkg]
requiredDeps = map Pkg ["MLSpec", "quickspec", "QuickCheck"]

requiredMods :: [Mod]
requiredMods = map Mod ["MLSpec.Helper", "Test.QuickSpec"]

renderMain :: String -> String
renderMain x = "main = Test.QuickSpec.quickSpec (MLSpec.Helper.addVars (" ++ x ++ "))"

asList :: [Expr] -> Expr
asList = foldr (\x -> (("(:)" $$ x) $$)) "[]"

renderDef :: [Entry] -> Expr
renderDef es = withPkgs ["quickspec"] (qualified "Test.QuickSpec" "signature") $$
               asList (concatMap theoryLine es)

getProjects :: String -> [Theory]
getProjects s = map theory (readClusters s)

readClusters :: String -> [Cluster]
readClusters x = fromMaybe [] (decode . fromString $ x)

runTheoriesFromClusters :: String -> IO [String]
runTheoriesFromClusters s = catMaybes <$> mapM runTheory (getProjects s)

runTheory :: Theory -> IO (Maybe String)
runTheory (T es) = eval' renderMain (renderDef es)
