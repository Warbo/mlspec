module TestData where

import Language.Eval
import MLSpec.Theory

clusters = [
    C [
        E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.StringSet"],
           eFlags = [],
           eExpr = "foldPS",
           ePreamble = []},
           Ty "(Data.StringMap.Types.Key -> b -> b)      -> b      -> (Data.StringMap.Types.Key -> Data.StringMap.Types.Key)      -> Data.StringMap.StringSet.StringSet      -> b",
           A 4,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.StringSet"],
           eFlags = [],
           eExpr = "foldWithKeyPS",
           ePreamble = []},
           Ty "(Data.StringMap.Types.Key -> b -> b)      -> b -> Data.StringMap.StringSet.StringSet -> b",
           A 3,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "val",
           ePreamble = []},
           Ty "v      -> Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 2,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "valueWithDefault",
           ePreamble = []},
           Ty "a -> Data.StringMap.Base.StringMap a -> a",
           A 2,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "findWithDefault",
           ePreamble = []},
           Ty "a      -> Data.StringMap.Types.Key -> Data.StringMap.Base.StringMap a -> a",
           A 3,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "lookupRange",
           ePreamble = []},
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "updateWithKey",
           ePreamble = []},
           Ty "(Data.StringMap.Types.Key -> a -> Maybe a)      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "adjust",
           ePreamble = []},
           Ty "(a -> a)      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "adjustWithKey",
           ePreamble = []},
           Ty "(Data.StringMap.Types.Key -> a -> a)      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "intersection",
           ePreamble = []},
           Ty "Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "cutAllPx'",
           ePreamble = []},
           Ty "Data.StringMap.StringSet.StringSet      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "prefixFind",
           ePreamble = []},
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a -> [a]",
           A 2,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "toList",
           ePreamble = []},
           Ty "Data.StringMap.Base.StringMap a      -> [(Data.StringMap.Types.Key, a)]",
           A 1,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "prefixFindWithKey",
           ePreamble = []},
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> [(Data.StringMap.Types.Key, a)]",
           A 2,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "tree",
           ePreamble = []},
           Ty "Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 1,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "singleton",
           ePreamble = []},
           Ty "Data.StringMap.Types.Key -> a -> Data.StringMap.Base.StringMap a",
           A 2,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "insertWith",
           ePreamble = []},
           Ty "(a -> a -> a)      -> Data.StringMap.Types.Key      -> a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 4,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "insertWithKey",
           ePreamble = []},
           Ty "(Data.StringMap.Types.Key -> a -> a -> a)      -> Data.StringMap.Types.Key      -> a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 4,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "fromList",
           ePreamble = []},
           Ty "[(Data.StringMap.Types.Key, a)]      -> Data.StringMap.Base.StringMap a",
           A 1,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "prefixFindWithKeyBF",
           ePreamble = []},
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> [(Data.StringMap.Types.Key, a)]",
           A 2,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Strict"],
           eFlags = [],
           eExpr = "singleton",
           ePreamble = []},
           Ty "Data.StringMap.Types.Key -> a -> Data.StringMap.Base.StringMap a",
           A 2,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Strict"],
           eFlags = [],
           eExpr = "insertWith",
           ePreamble = []},
           Ty "(a -> a -> a)      -> Data.StringMap.Types.Key      -> a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 4,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Strict"],
           eFlags = [],
           eExpr = "insert",
           ePreamble = []},
           Ty "Data.StringMap.Types.Key      -> a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Strict"],
           eFlags = [],
           eExpr = "fromList",
           ePreamble = []},
           Ty "[(Data.StringMap.Types.Key, a)]      -> Data.StringMap.Base.StringMap a",
           A 1,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Strict"],
           eFlags = [],
           eExpr = "insertWithKey",
           ePreamble = []},
           Ty "(Data.StringMap.Types.Key -> a -> a -> a)      -> Data.StringMap.Types.Key      -> a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 4,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Strict"],
           eFlags = [],
           eExpr = "updateWithKey",
           ePreamble = []},
           Ty "(Data.StringMap.Types.Key -> a -> Maybe a)      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Strict"],
           eFlags = [],
           eExpr = "adjust",
           ePreamble = []},
           Ty "(a -> a)      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Strict"],
           eFlags = [],
           eExpr = "adjustWithKey",
           ePreamble = []},
           Ty "(Data.StringMap.Types.Key -> a -> a)      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Dim2Search"],
           eFlags = [],
           eExpr = "lookupRange",
           ePreamble = []},
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3,
           H False)
      ],
    C [
        E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.StringSet"],
           eFlags = [],
           eExpr = "elemsPS",
           ePreamble = []},
           Ty "Data.StringMap.StringSet.StringSet -> [Data.StringMap.Types.Key]",
           A 1,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.StringSet"],
           eFlags = [],
           eExpr = "nullPS",
           ePreamble = []},
           Ty "Data.StringMap.StringSet.StringSet -> Bool",
           A 1,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "empty",
           ePreamble = []},
           Ty "Data.StringMap.Base.StringMap v",
           A 0,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "unNorm",
           ePreamble = []},
           Ty "Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 1,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "deepUnNorm",
           ePreamble = []},
           Ty "Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 1,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "!",
           ePreamble = []},
           Ty "Data.StringMap.Base.StringMap a -> Data.StringMap.Types.Key -> a",
           A 2,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "update",
           ePreamble = []},
           Ty "(a -> Maybe a)      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "delete",
           ePreamble = []},
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "union",
           ePreamble = []},
           Ty "Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "unionWith",
           ePreamble = []},
           Ty "(a -> a -> a)      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "unionWithKey",
           ePreamble = []},
           Ty "(Data.StringMap.Types.Key -> a -> a -> a)      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "cutPx'",
           ePreamble = []},
           Ty "Data.StringMap.StringSet.StringSet      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "elems",
           ePreamble = []},
           Ty "Data.StringMap.Base.StringMap a -> [a]",
           A 1,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "child",
           ePreamble = []},
           Ty "Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 1,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "toMap",
           ePreamble = []},
           Ty "Data.StringMap.Base.StringMap a      -> containers-0.5.6.2:Data.Map.Base.Map Data.StringMap.Types.Key a",
           A 1,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "fromKey",
           ePreamble = []},
           Ty "Data.StringMap.Types.Key -> Data.StringMap.Base.Key1",
           A 1,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "insert",
           ePreamble = []},
           Ty "Data.StringMap.Types.Key      -> a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "fromMap",
           ePreamble = []},
           Ty "containers-0.5.6.2:Data.Map.Base.Map Data.StringMap.Types.Key a      -> Data.StringMap.Base.StringMap a",
           A 1,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.FuzzySearch"],
           eFlags = [],
           eExpr = "lookupNoCase",
           ePreamble = []},
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.FuzzySearch"],
           eFlags = [],
           eExpr = "prefixFilterNoCase",
           ePreamble = []},
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.FuzzySearch"],
           eFlags = [],
           eExpr = "prefixFilter",
           ePreamble = []},
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Strict"],
           eFlags = [],
           eExpr = "update",
           ePreamble = []},
           Ty "(a -> Maybe a)      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Strict"],
           eFlags = [],
           eExpr = "delete",
           ePreamble = []},
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Strict"],
           eFlags = [],
           eExpr = "union",
           ePreamble = []},
           Ty "Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Strict"],
           eFlags = [],
           eExpr = "unionWith",
           ePreamble = []},
           Ty "(a -> a -> a)      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Dim2Search"],
           eFlags = [],
           eExpr = "lookupLE",
           ePreamble = []},
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Dim2Search"],
           eFlags = [],
           eExpr = "lookupGE",
           ePreamble = []},
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2,
           H False)
      ]
    , C [
        E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "normError'",
           ePreamble = []},
           Ty "String -> String -> a",
           A 2,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "toKey",
           ePreamble = []},
           Ty "Data.StringMap.Base.Key1 -> Data.StringMap.Types.Key",
           A 1,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "branch",
           ePreamble = []},
           Ty "Data.StringMap.Types.Sym      -> Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 3,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "norm",
           ePreamble = []},
           Ty "Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 1,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "lookupGE",
           ePreamble = []},
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "lookupLE",
           ePreamble = []},
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "toListShortestFirst",
           ePreamble = []},
           Ty "Data.StringMap.Base.StringMap v      -> [(Data.StringMap.Types.Key, v)]",
           A 1,
           H False)
      ]
    , C [
        E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "siseq",
           ePreamble = []},
           Ty "Data.StringMap.Base.Key1      -> Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 2,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "deepNorm",
           ePreamble = []},
           Ty "Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 1,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "next",
           ePreamble = []},
           Ty "Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 1,
           H False)
      , E (Expr {ePkgs = [Pkg "data-stringmap"],
           eMods = [Mod "Data.StringMap.Base"],
           eFlags = [],
           eExpr = "value'",
           ePreamble = []},
           Ty "Data.StringMap.Base.StringMap v -> v",
           A 1,
           H False)
      ]
    ]
