module Helper where
import Data.Char
import Language.Haskell.TH.Syntax
import Test.QuickCheck.All
mono = fmap vToC . monomorphic
vToC (VarE n) = if isC then ConE n else VarE n
  where isC   = isUpper c || c `elem` ":["
        (c:_) = nameBase n

-- FIXME: return makes for a pretty crappy generator...
addVars sig = sig:vs
  where vs = [gvars (names (witness w)) (return (witness w)) |
                Some w <-         argumentTypes sig,
                Some w `elem`    inhabitedTypes sig,
                Some w `notElem`  variableTypes sig]
        names x = let n = show (typeRep [x])
                   in [n ++ "1", n ++ "2", n ++ "3"]
