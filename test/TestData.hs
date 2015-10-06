module TestData where

import Language.Eval
import MLSpec.Theory

clusters = [
    C [
        E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.StringSet"],
           eFlags = [],
           eExpr = "foldPS"},
           Ty "(Data.StringMap.Types.Key -> b -> b)      -> b      -> (Data.StringMap.Types.Key -> Data.StringMap.Types.Key)      -> Data.StringMap.StringSet.StringSet      -> b",
           A 4)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.StringSet"],
           eFlags = [],
           eExpr = "foldWithKeyPS"},
           Ty "(Data.StringMap.Types.Key -> b -> b)      -> b -> Data.StringMap.StringSet.StringSet -> b",
           A 3)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "val"},
           Ty "v      -> Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 2)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "valueWithDefault"},
           Ty "a -> Data.StringMap.Base.StringMap a -> a",
           A 2)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "findWithDefault"},
           Ty "a      -> Data.StringMap.Types.Key -> Data.StringMap.Base.StringMap a -> a",
           A 3)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "lookupRange"},
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "updateWithKey"},
           Ty "(Data.StringMap.Types.Key -> a -> Maybe a)      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "adjust"},
           Ty "(a -> a)      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "adjustWithKey"},
           Ty "(Data.StringMap.Types.Key -> a -> a)      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "intersection"},
           Ty "Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "cutAllPx'"},
           Ty "Data.StringMap.StringSet.StringSet      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "prefixFind"},
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a -> [a]",
           A 2)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "toList"},
           Ty "Data.StringMap.Base.StringMap a      -> [(Data.StringMap.Types.Key, a)]",
           A 1)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "prefixFindWithKey"},
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> [(Data.StringMap.Types.Key, a)]",
           A 2)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "tree"},
           Ty "Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 1)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "singleton"},
           Ty "Data.StringMap.Types.Key -> a -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "insertWith"},
           Ty "(a -> a -> a)      -> Data.StringMap.Types.Key      -> a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 4)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "insertWithKey"},
           Ty "(Data.StringMap.Types.Key -> a -> a -> a)      -> Data.StringMap.Types.Key      -> a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 4)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "fromList"},
           Ty "[(Data.StringMap.Types.Key, a)]      -> Data.StringMap.Base.StringMap a",
           A 1)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "prefixFindWithKeyBF"},
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> [(Data.StringMap.Types.Key, a)]",
           A 2)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Strict"],
           eFlags = [],
           eExpr = "singleton"},
           Ty "Data.StringMap.Types.Key -> a -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Strict"],
           eFlags = [],
           eExpr = "insertWith"},
           Ty "(a -> a -> a)      -> Data.StringMap.Types.Key      -> a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 4)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Strict"],
           eFlags = [],
           eExpr = "insert"},
           Ty "Data.StringMap.Types.Key      -> a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Strict"],
           eFlags = [],
           eExpr = "fromList"},
           Ty "[(Data.StringMap.Types.Key, a)]      -> Data.StringMap.Base.StringMap a",
           A 1)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Strict"],
           eFlags = [],
           eExpr = "insertWithKey"},
           Ty "(Data.StringMap.Types.Key -> a -> a -> a)      -> Data.StringMap.Types.Key      -> a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 4)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Strict"],
           eFlags = [],
           eExpr = "updateWithKey"},
           Ty "(Data.StringMap.Types.Key -> a -> Maybe a)      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Strict"],
           eFlags = [],
           eExpr = "adjust"},
           Ty "(a -> a)      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Strict"],
           eFlags = [],
           eExpr = "adjustWithKey"},
           Ty "(Data.StringMap.Types.Key -> a -> a)      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Dim2Search"],
           eFlags = [],
           eExpr = "lookupRange"},
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      ],
    C [
        E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.StringSet"],
           eFlags = [],
           eExpr = "elemsPS"},
           Ty "Data.StringMap.StringSet.StringSet -> [Data.StringMap.Types.Key]",
           A 1)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.StringSet"],
           eFlags = [],
           eExpr = "nullPS"},
           Ty "Data.StringMap.StringSet.StringSet -> Bool",
           A 1)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "empty"},
           Ty "Data.StringMap.Base.StringMap v",
           A 0)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "unNorm"},
           Ty "Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 1)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "deepUnNorm"},
           Ty "Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 1)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "!"},
           Ty "Data.StringMap.Base.StringMap a -> Data.StringMap.Types.Key -> a",
           A 2)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "update"},
           Ty "(a -> Maybe a)      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "delete"},
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "union"},
           Ty "Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "unionWith"},
           Ty "(a -> a -> a)      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "unionWithKey"},
           Ty "(Data.StringMap.Types.Key -> a -> a -> a)      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "cutPx'"},
           Ty "Data.StringMap.StringSet.StringSet      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "elems"},
           Ty "Data.StringMap.Base.StringMap a -> [a]",
           A 1)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "child"},
           Ty "Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 1)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "toMap"},
           Ty "Data.StringMap.Base.StringMap a      -> containers-0.5.6.2:Data.Map.Base.Map Data.StringMap.Types.Key a",
           A 1)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "fromKey"},
           Ty "Data.StringMap.Types.Key -> Data.StringMap.Base.Key1",
           A 1)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "insert"},
           Ty "Data.StringMap.Types.Key      -> a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "fromMap"},
           Ty "containers-0.5.6.2:Data.Map.Base.Map Data.StringMap.Types.Key a      -> Data.StringMap.Base.StringMap a",
           A 1)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.FuzzySearch"],
           eFlags = [],
           eExpr = "lookupNoCase"},
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.FuzzySearch"],
           eFlags = [],
           eExpr = "prefixFilterNoCase"},
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.FuzzySearch"],
           eFlags = [],
           eExpr = "prefixFilter"},
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Strict"],
           eFlags = [],
           eExpr = "update"},
           Ty "(a -> Maybe a)      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Strict"],
           eFlags = [],
           eExpr = "delete"},
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Strict"],
           eFlags = [],
           eExpr = "union"},
           Ty "Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Strict"],
           eFlags = [],
           eExpr = "unionWith"},
           Ty "(a -> a -> a)      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Dim2Search"],
           eFlags = [],
           eExpr = "lookupLE"},
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Dim2Search"],
           eFlags = [],
           eExpr = "lookupGE"},
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      ]
    , C [
        E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "normError'"},
           Ty "String -> String -> a",
           A 2)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "toKey"},
           Ty "Data.StringMap.Base.Key1 -> Data.StringMap.Types.Key",
           A 1)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "branch"},
           Ty "Data.StringMap.Types.Sym      -> Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 3)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "norm"},
           Ty "Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 1)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "lookupGE"},
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "lookupLE"},
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "toListShortestFirst"},
           Ty "Data.StringMap.Base.StringMap v      -> [(Data.StringMap.Types.Key, v)]",
           A 1)
      ]
    , C [
        E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "siseq"},
           Ty "Data.StringMap.Base.Key1      -> Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 2)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "deepNorm"},
           Ty "Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 1)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "next"},
           Ty "Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 1)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "value'"},
           Ty "Data.StringMap.Base.StringMap v -> v",
           A 1)
      ]
    ]
