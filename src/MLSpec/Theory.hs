{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module MLSpec.Theory where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char
import           Data.Data
import           Data.Generics.Aliases
import           Data.List
import           Data.Maybe
import qualified Data.MemoTrie as MT
import           Data.Stringable
import           Data.Typeable
import           Language.Eval
import           Language.Eval.Internal
import qualified Language.Haskell.Exts.Parser as HEP
import qualified Language.Haskell.Exts.Syntax as HES
import           System.IO
import           System.IO.Unsafe
import           System.Process

newtype Type  = Ty String deriving (Eq, Ord)
newtype Arity = A  Int    deriving (Eq, Ord)
newtype Hashable = H Bool deriving (Eq, Ord, Show)

unPkg  (Pkg  x) = x
unMod  (Mod  x) = x
unFlag (Flag x) = x
unType (Ty   x) = x

newtype Entry = E (Expr, Type, Arity, Hashable) deriving (Show, Eq)

instance FromJSON Entry where
  parseJSON (Object o) = do
    p <- o .: "package"
    m <- o .: "module"
    n <- o .: "name"
    t <- o .: "type"
    a <- o .: "arity"
    h <- o .: "hashable"
    return $ E (withPkgs [Pkg p] $ withMods [Mod m] $ raw n, Ty t, A a, H h)
  parseJSON x = fail ("Doesn't look like an 'Entry': " ++ show x)

instance ToJSON Entry where
  toJSON (E (e, Ty t, A a, H h)) = object [
      "package"  .= unPkg (head (ePkgs e))
    , "module"   .= unMod (head (eMods e))
    , "name"     .= eExpr e
    , "type"     .= t
    , "arity"    .= a
    , "hashable" .= h
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
getExpr (E (e, _, _, _)) = e

getPkg :: Entry -> [Pkg]
getPkg = ePkgs . getExpr

getMod :: Entry -> [Mod]
getMod = eMods . getExpr

getName :: Entry -> String
getName = eExpr . getExpr

getType :: Entry -> Type
getType (E (_, t, _, _)) = t

getArity :: Entry -> Arity
getArity (E (_, _, a, _)) = a

getHashable (E (_, _, _, h)) = h

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

-- If the entry is hashable, we use a "blind" function and an observer.
-- Otherwise we use a "fun" function.
theoryLine :: Entry -> [Expr]
theoryLine (E (_, _, A a, _  )) | a > 5 = []  -- QuickSpec only goes up to fun5
theoryLine (E (e, _, A a, H h))         = [letIn [(name, val)] x]
  where name = "f"
        val  = thUnquote (mono $$ thQuote (wrapOp e))
        mono = withPkgs ["mlspec-helper"] $ qualified "MLSpec.Helper" "mono"
        x    = if h then blind else fun

        fun  = (ofArity "fun" $$ symbol) $$ name

        blind = signature $$
                  (("(:)" $$ ((ofArity "blind" $$ symbol) $$ name)) $$
                  (("(:)" $$ (observer $$ hasher)) $$ "[]"))

        -- Suffix func with arity, e.g. fun0, fun1, blind0, blind1, etc.
        ofArity func = qualified "Test.QuickSpec" (wrapped "" (show a) func)

        -- The "name" we tell QuickSpec to use; the Haskell global's name
        symbol = quoted (raw (eExpr e))

        signature = withPkgs ["quickspec"]
                      (qualified "Test.QuickSpec.Signature" "signature")

        observer = withPkgs ["quickspec"]
                     (qualified "Test.QuickSpec.Signature" "observer1")

        -- Gives us an expression of f's output type
        addCalls 0 x = x
        addCalls n x = addCalls (n-1) (x $$ "undefined")
        fOut = addCalls a "f"

        -- Forces input type to be f's output type
        converter = ("flip" $$ "asTypeOf") $$ fOut

        compose [x] = x
        compose (x:xs) = ("(.)" $$ x) $$ compose xs

        hasher = compose [withPkgs ["murmur-hash"]
                            (qualified "Data.Digest.Murmur32" "asWord32"),
                          withPkgs ["murmur-hash"]
                            (qualified "Data.Digest.Murmur32" "hash32"),
                          withPkgs ["cereal"]
                            (qualified "Data.Serialize" "runPut"),
                          withPkgs ["cereal"]
                            (qualified "Data.Serialize" "put"),
                          converter]

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
      ePkgs     = nub ps'
    , eMods     = nub ms'
    , eFlags    = nub fs'
    , ePreamble = pre
    , eExpr     = expr
    }
  where ps'  = ePkgs     e ++ concatMap ePkgs     nves
        ms'  = eMods     e ++ concatMap eMods     nves
        fs'  = eFlags    e ++ concatMap eFlags    nves
        pre  = ePreamble e ++ concatMap ePreamble nves
        x    = eExpr e
        nves = map fst nvs ++ map snd nvs
        expr = "let {" ++ intercalate ";" defs ++ "} in (" ++ x ++ ")"
        defs = map mkDef nvs
        mkDef (n, v) = "(" ++ eExpr n ++ ") = (" ++ eExpr v ++ ")"

theory :: Cluster -> Theory
theory (C es) = T (nub es)

addScope = withPkgs requiredDeps . withMods requiredMods

requiredDeps :: [Pkg]
requiredDeps = map Pkg ["mlspec-helper", "quickspec", "QuickCheck",
                        "runtime-arbitrary", "hashable", "murmur-hash",
                        "cereal"]

requiredMods :: [Mod]
requiredMods = map Mod ["Data.Serialize", "Data.Digest.Murmur32",
                        "MLSpec.Helper", "Test.QuickSpec",
                        "Test.RuntimeArbitrary"]

withoutUndef' :: String -> String
withoutUndef' x = "(Test.QuickSpec.without (" ++ x ++ ") [\"undefined\"])"

without' :: Expr
without' = qualified "Test.QuickSpec" "without"

withoutUndefExpr :: Expr -> Expr
withoutUndefExpr e = (without' $$ e) $$ "[\"undefined\"]"

quickSpec' :: String -> String
quickSpec' x = "MLSpec.Helper.quickSpecRaw (" ++ x ++ ")"

quickSpecPrint' :: String -> String
quickSpecPrint' x =
  "do { eqs <- " ++ quickSpec' x ++ "; Prelude.putStrLn (Prelude.unlines (Prelude.map Prelude.show eqs)); }"

renderMain :: [String] -> String -> String
renderMain ts x = "main = " ++ quickSpecPrint' (withoutUndef' (renderWithVariables x ts))

renderMainShowVarTypes :: String -> String
renderMainShowVarTypes x = "main = " ++ intercalate " Prelude.>> " [
    pre',
    "Prelude.putStrLn (MLSpec.Helper.showReqVarTypes (" ++ withoutUndef' x ++ "))",
    post']
  where pre'  = "Prelude.putStrLn \"BEGIN TYPES\""
        post' = "Prelude.putStrLn \"END TYPES\""

getRequiredTypes :: [Theory] -> IO [([String], Expr)]
getRequiredTypes [] = return []
getRequiredTypes ts = do
    Just stdout <- eval' ("main = " ++)
                         (renderMainShowVarTypesMultiple rendered)
    return (extractAllTypes rendered stdout)
  where rendered      = map render ts
        render (T es) = renderDef es

putStrLn' :: Expr
putStrLn' = "Prelude.putStrLn"

seq' []  = error "Nothing to sequence"
seq' [x] = x
seq' (x:xs) = ("Prelude.>>" $$ x) $$ seq' xs

renderMainShowVarTypesMultiple :: [Expr] -> Expr
renderMainShowVarTypesMultiple = go 0
  where go idx []     = putStrLn' $$ "\"\""
        go idx (x:xs) =
          let pre = putStrLn' $$ asString ("BEGIN TYPES " ++ show idx)
              suf = putStrLn' $$ asString ("END TYPES "   ++ show idx)
              x'  = putStrLn' $$ (qualified "MLSpec.Helper" "showReqVarTypes" $$
                                    withoutUndefExpr x)
              xs' = renderMainShowVarTypesMultiple xs
           in seq' [pre, x', suf, xs']

-- Can't find a way to qualify ":" and "[]", so leave them bare
asList :: [Expr] -> Expr
asList = foldr (\x -> (("(:)" $$ x) $$)) "[]"

renderDef :: [Entry] -> Expr
renderDef es = addScope (withInstances sig)
  where signature' = withPkgs ["quickspec"] . qualified "Test.QuickSpec" $
                       "signature"
        sig        = case es of
                          [] -> qualified "Test.QuickSpec.Signature" "emptySig"
                          _  -> signature' $$ asList (concatMap theoryLine es)
        withInstances = withPkgs ["runtime-arbitrary", "QuickCheck", "ifcxt"] .
                        withMods ["IfCxt", "Test.QuickCheck"]                 .
                        withFlags flags
        flags      = ["-XTemplateHaskell",
                      "-XFlexibleInstances",
                      "-XFlexibleContexts",
                      "-XScopedTypeVariables"]

getProjects :: String -> [Theory]
getProjects s = map theory (readClusters s)

readClusters :: String -> [Cluster]
readClusters x = case eitherDecode' (fromString x) of
    Right cs -> cs
    Left  e1 -> case eitherDecode' (fromString x) of
      Right c  -> [c]
      Left  e2 -> error (report e1 e2)
  where report :: String -> String -> String
        report e1 e2 = concat [
          "Failed to read list of clusters, with message: ", e1,
          "\n\nTried to read a single cluster, but failed with message: ", e2,
          "\n\nGiving up!"]

runTheoriesFromClusters :: String -> IO String
runTheoriesFromClusters = fmap unDepth .  runTheories . getProjects

unDepth :: String -> String
unDepth = unlines . filter noDepth . lines
  where noDepth :: String -> Bool
        noDepth "" = False
        noDepth l  = not ("Depth " `isPrefixOf` l)

runTheories :: [Theory] -> IO String
runTheories [] = return ""
runTheories ts = do
  withTypes <- getRequiredTypes (filter (\(T es) -> not (null es)) ts)
  case withTypes of
       [] -> return ""
       _  -> do
         result <- eval' (\x -> "main = do { " ++ x ++ "}")
                         (renderMains withTypes)
         case result of
              Nothing -> error "Error running theories"
              Just x  -> return x

-- Combines a list of Expr together; the expression will be meaningless, but is
-- useful for combining dependencies
combineDeps :: [Expr] -> Expr
combineDeps = foldr ($$) ""

renderMains :: [([String], Expr)] -> Expr
renderMains xs = expr {
                   eExpr = "putStrLn \"\"; " ++ joined
                 }
  where expr = withPkgs ["mlspec-helper", "quickspec"]      .
               withMods ["MLSpec.Helper", "Test.QuickSpec"] .
               withInstances . combineDeps . map snd $ xs
        joined :: String
        joined = intercalate "; " . map renderEach $ xs
        renderEach :: ([String], Expr) -> String
        renderEach (ts, x) = quickSpecPrint'
                               (withoutUndef' (renderWithVariables (eExpr x) ts))

withInstances = withPreamble "mkIfCxtInstances ''Arbitrary"            .
                withPreamble "mkIfCxtInstances ''CoArbitrary"          .
                withPreamble "mkIfCxtInstances ''Ord"                  .
                withPreamble "mkIfCxtInstances ''Test.Feat.Enumerable" .
                withPkgs ["testing-feat"]                              .
                withMods ["Test.Feat"]

-- | Like renderTheory', but includes preamble for defining instances. We keep
--   this separate, since instances should be defined exactly once, and hence
--   this function's results can't be combined.
renderTheory :: Theory -> IO (Maybe ([String], Expr))
renderTheory t = do
  x <- renderTheory' t
  case x of
    Nothing -> return Nothing
    Just (ts, e) -> return (Just (ts, withInstances e))

-- | Returns `Nothing` for an empty `Theory`, otherwise renders an Expr
--   containing a signature for the given `Theory`.
renderTheory' :: Theory -> IO (Maybe ([String], Expr))
renderTheory' (T []) = return Nothing
renderTheory' (T es) = do
    Just stdout <- eval' renderMainShowVarTypes rendered
    let (types, mods, pkgs) = extractTypesFromOutput stdout
    return (Just (types, withPkgs pkgs (withMods mods rendered)))
  where rendered = renderDef es

renderWithVariables :: String -> [String] -> String
renderWithVariables sig ts = "(" ++ addVars' ts sig ++ ")"

addVars' :: [String] -> String -> String
addVars' []     x = x
addVars' (t:ts) x = concat [
  "\n(MLSpec.Helper.addVars \n",
  show t,
  "\n (Test.RuntimeArbitrary.getArbGen [Prelude.undefined :: " ++ t ++ "])",
  "\n (" ++ addVars' ts x ++ "))"]

extractTypesFromOutput :: String -> ([String], [Mod], [Pkg])
extractTypesFromOutput = extractSection .
                         dropStart      .
                         dropEnd        .
                         lines
  where dropStart =           drop 1 . dropWhile (/= "BEGIN TYPES")
        dropEnd   = reverse . drop 1 . dropWhile (/= "END TYPES") . reverse

extractSection :: [String] -> ([String], [Mod], [Pkg])
extractSection = collate                    .
                 mapMaybe jsonToTypeModPkgs .
                 filter looksJson
  where looksJson ('{':_) = True
        looksJson _       = False
        collate [] = ([], [], [])
        collate ((s, ms, ps):xs) = let (ss, ms', ps') = collate xs
                                    in (s:ss, ms++ms', ps++ps')

extractAllTypes :: [Expr] -> String -> [([String], Expr)]
extractAllTypes es s = zipWith augment outputs es
  where -- Give each signature a list of types which we should provide variables
        -- for. Also ensure the signature depends on mods/pkgs required for
        -- those types (e.g. if they're in a separate ".Types" module)
        augment o e = case extractSection o of
          (types, mods, pkgs) -> (types, withPkgs pkgs (withMods mods e))

        -- The sections of text between "BEGIN TYPES" and "END TYPES" sentinels
        outputs = splitString False [] (lines s)

        -- Parses lines to extract the 'sections'. We recurse through the lines,
        -- building up an accumulator of the sections. The 'keep' parameter
        -- determines whether we accumulate or skip the line, and we toggle this
        -- parameter when we spot the sentinels.
        splitString :: Bool -> [[String]] -> [String] -> [[String]]

        -- If we see a BEGIN sentinel, start keeping lines in a new acc
        splitString   False     acc  (l:ls) | "BEGIN TYPES" `isPrefixOf` l =
          splitString True  ([]:acc)    ls

        -- If we see an END sentinel, stop keeping lines and finalise acc's head
        splitString   True          (a:acc) (l:ls) | "END TYPES" `isPrefixOf` l =
          splitString False (reverse a:acc)    ls

        -- Base case. If there are no more lines, finalise acc and return it.
        splitString   False acc     [] = reverse acc

        -- If we're keeping lines, prepend one to acc's head and recurse
        splitString   True     (a:acc) (l:ls) =
          splitString True ((l:a):acc)    ls

        -- If we're not keeping lines, skip and recurse
        splitString   False acc  (l:ls) =
          splitString False acc     ls

        -- No other patterns are permitted (e.g. keeping when there's no acc)
        splitString   keep  acc     ls = error . concat $ [
            "Unexpected situation arose while parsing. Accumulated so far: ",
            show acc,
            ". Lines remaining: ", show ls, ".",
            "Currently ", if keep then "keeping" else "skipping", " lines."
          ]

jsonToTypeModPkgs :: String -> Maybe (String, [Mod], [Pkg])
jsonToTypeModPkgs s = case jsonToTypeModPkgs' s of
  Left  e -> error e
  Right t -> t

jsonToTypeModPkgs' :: String -> Either String (Maybe (String, [Mod], [Pkg]))
jsonToTypeModPkgs' s = eitherDecode' (fromString s) >>= parseEither parseTypeModPkgs
  where parseTypeModPkgs x = do
          args   <- x  .: "args"
          tc     <- x  .: "tycon"
          tcName <- tc .: "name"
          tcMod  <- tc .: "module"
          tcPkg  <- tc .: "package"
          return $ case switchHidden tcName (Mod tcMod) (Pkg tcPkg) args of
            Nothing -> err (show ("switchHidden", tcName, tcMod, tcPkg, args))
                           Nothing
            Just (tcPkg', tcMod', name) ->
              if not (keepPkg (Pkg tcPkg)) || tcMod' `exposedInPkg` tcPkg'
                 then do mods <- mapM argMod args
                         pkgs <- mapM argPkg args
                         Just (name,
                               nub $ tcMod' : concat mods,
                               nub $ filter keepPkg (tcPkg' : concat pkgs))
                 else err ("Ignoring hidden: " ++ show (tcMod', name))
                      Nothing

-- Switch some known types whose canonical name is hidden, and therefore cannot
-- be imported. Since we use Integer to monomorphise, we're basically forced to
-- special-case it.
switchHidden :: String -> Mod -> Pkg -> [Value] -> Maybe (Pkg, Mod, String)
switchHidden "Integer" (Mod "GHC.Integer.Type") (Pkg "integer-gmp") [] =
  Just ("base", "Prelude", "Prelude.Integer")

switchHidden "[]" (Mod "GHC.Types") (Pkg "ghc-prim") [a] =
  case renderArg a of
       Just a' -> Just ("ghc-prim", "GHC.Types", "[" ++ a' ++ "]")
       Nothing -> err (show ("renderArg failed", a)) Nothing

switchHidden "(->)" (Mod "GHC.Prim") (Pkg "ghc-prim") [a, b] =
  case (renderArg a, renderArg b) of
       (Just a', Just b') -> Just ("ghc-prim",
                                   "GHC.Types",
                                   concat ["((->) ", a', " ", b', ")"])
       _                  -> err (show ("renderArg failed", a, b)) Nothing

switchHidden n (Mod "GHC.Tuple") p args | all (`elem` ("()," :: String)) n =
  case mapM renderArg args of
       Just args' -> Just ("ghc-prim",
                           "GHC.Tuple",
                           concat ["(", unwords (n:args'), ")"])
       _          -> err (show ("renderArg failed", n, args)) Nothing

switchHidden n (Mod m) p args =
  case typeString m n args of
       Just t  -> Just (p, Mod m, t)
       Nothing -> err (show ("typeString failed", m, n, args)) Nothing

typeString :: String -> String -> [Value] -> Maybe String
typeString mod name args =
    if null args
       then Just tcString
       else wrap <$> (unwords <$> rArgs)
  where tcString = "(" ++ mod ++ "." ++ name ++ ")" :: String
        rArgs    = mapM renderArg args
        wrap x   = "(" ++ tcString ++ " " ++ x ++ ")"

renderArg :: Value -> Maybe String
renderArg v = case jsonToTypeModPkgs . toString . encode $ v of
                   Just (s, _,  _) -> Just s
                   Nothing         -> err (show ("renderArg",
                                                 ("v", v),
                                                 ("encode v", encode v)))
                                          Nothing
argMod    v = case jsonToTypeModPkgs . toString . encode $ v of
                   Just (_, ms, _) -> Just ms
                   Nothing         -> Nothing
argPkg    v = case jsonToTypeModPkgs . toString . encode $ v of
                   Just (_, _, ps) -> Just ps
                   Nothing         -> Nothing

-- Whether to use a given package name. '_' indicates a GHC package DB key,
-- which isn't portable across GHC environments, so we discard them.
keepPkg (Pkg p) = '_' `notElem` p

exposedInPkg (Mod m) (Pkg p) = exposedInPkgM (m, p)

exposedInPkgM :: (String, String) -> Bool
exposedInPkgM = MT.memo exposedInPkg'

exposedInPkg' (mod, pkg) = mod `elem` modlist
  where output  = readProcess "ghc-pkg" ["field", pkg, "exposed-modules"] ""
        output' = unsafePerformIO output
        modlist = words . unlines . filter ((== " ") . take 1) . lines $ output'

err :: String -> a -> a
err m x = unsafePerformIO $ do
            hPutStrLn stderr m
            return x
