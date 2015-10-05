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

newtype Type  = Ty String deriving (Eq, Ord)
newtype Arity = A  Int    deriving (Eq, Ord)

unPkg  (Pkg x) = x
unMod  (Mod x) = x
unType (Ty  x) = x

newtype Entry = E (Expr, Type, Arity) deriving (Show, Eq)

instance FromJSON Entry where
  parseJSON (Object o) = do
    p <- o .: "package"
    m <- o .: "module"
    n <- o .: "name"
    t <- o .: "type"
    a <- o .: "arity"
    return $ E (withPkgs [Pkg p] (qualified (Mod m) (raw n)), Ty t, A a)
  parseJSON _ = mzero

instance ToJSON Entry where
  toJSON (E (Expr (Pkg p:_, Mod m:_, n), Ty t, A a)) = object [
      "package" .= p
    , "module"  .= (init . reverse . dropWhile (/= '.') . reverse $ m)
    , "name"    .=        (reverse . takeWhile (/= '.') . reverse $ n)
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

pkgsOf :: Expr -> [Pkg]
pkgsOf (Expr (ps, _, _)) = ps

modsOf :: Expr -> [Mod]
modsOf (Expr (_, ms, _)) = ms

exprOf :: Expr -> String
exprOf (Expr (_, _, e))  = e

getExpr :: Entry -> Expr
getExpr (E (e, _, _)) = e

getPkg :: Entry -> [Pkg]
getPkg = pkgsOf . getExpr

getMod :: Entry -> [Mod]
getMod = modsOf . getExpr

getName :: Entry -> String
getName = exprOf . getExpr

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
        val  = thUnquote (qualified "Helper" "mono" $$ thQuote (wrapOp e))
        x    = func $$ quoted (asString e) $$ name
        func = qualified "Test.QuickSpec" (wrapped "" (show a) "fun")

wrapOp :: Expr -> Expr
wrapOp x@(Expr (ps, ms, e)) = if op e then parens x
                                      else x
  where op          = any isSym
        isSym c     = or [c `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String),
                          isPunctuation c,
                          isSymbol      c]

thQuote :: Expr -> Expr
thQuote (Expr (p, m, e)) = Expr (p, m, "'" ++ e)

wrapped :: String -> String -> Expr -> Expr
wrapped o c (Expr (p, m, e)) = Expr (p, m, o ++ e ++ c)

parens :: Expr -> Expr
parens = wrapped "(" ")"

thUnquote :: Expr -> Expr
thUnquote = wrapped "$(" ")"

quoted :: Expr -> Expr
quoted = wrapped "\"" "\""

-- | Let expressions. We use Expr on the left-hand-side, to allow
--   pattern-matching on constructors from arbitrary packages.
letIn :: [(Expr, Expr)] -> Expr -> Expr
letIn []                 x   = x
letIn nvs (Expr (ps, ms, x)) = Expr (nub ps', nub ms', expr)
  where ps'  = ps ++ concatMap pkgsOf nves
        ms'  = ms ++ concatMap modsOf nves
        nves = map fst nvs ++ map snd nvs
        expr = "let {" ++ intercalate ";" defs ++ "} in (" ++ x ++ ")"
        defs = map mkDef nvs
        mkDef (Expr (_, _, n), Expr (_, _, v)) = "(" ++ n ++ ") = (" ++ v ++ ")"


theory :: Cluster -> Theory
theory (C es) = T (nub es)

addTypeMods :: Theory -> Theory
addTypeMods (T ss) = T ss

addScope = withPkgs requiredDeps . withMods requiredMods

requiredDeps :: [Pkg]
requiredDeps = map Pkg ["MLSpec", "quickspec", "QuickCheck"]

requiredMods :: [Mod]
requiredMods = map Mod ["MLSpec.Helper", "Test.QuickSpec"]

renderMain :: String -> String
renderMain x = "main = Test.QuickSpec.quickSpec (Helper.addVars (" ++ x ++ "))"

asList :: [Expr] -> Expr
asList []     = "[]"
asList (x:xs) = ("(:)" $$ x) $$ asList xs

renderDef :: [Entry] -> Expr
renderDef es = qualified "Test.QuickSpec" "signature" $$
               asList (concatMap theoryLine es)

getProjects :: String -> [Theory]
getProjects s = map theory (readClusters s)

readClusters :: String -> [Cluster]
readClusters x = fromMaybe [] (decode . fromString $ x)

runTheoriesFromClusters :: String -> IO [String]
runTheoriesFromClusters s = catMaybes <$> mapM runTheory (getProjects s)

runTheory :: Theory -> IO (Maybe String)
runTheory (T es) = eval (renderDef es)
