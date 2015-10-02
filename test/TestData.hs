module TestData where

import Language.Eval
import MLSpec.Theory

clusters = [
    C [
        E (Pkg "data-stringmap",
           Mod "Data.StringMap.StringSet",
           N "foldPS",
           Ty "(Data.StringMap.Types.Key -> b -> b)      -> b      -> (Data.StringMap.Types.Key -> Data.StringMap.Types.Key)      -> Data.StringMap.StringSet.StringSet      -> b",
           A 4)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.StringSet",
           N "foldWithKeyPS",
           Ty "(Data.StringMap.Types.Key -> b -> b)      -> b -> Data.StringMap.StringSet.StringSet -> b",
           A 3)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "val",
           Ty "v      -> Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 2)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "valueWithDefault",
           Ty "a -> Data.StringMap.Base.StringMap a -> a",
           A 2)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "findWithDefault",
           Ty "a      -> Data.StringMap.Types.Key -> Data.StringMap.Base.StringMap a -> a",
           A 3)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "lookupRange",
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "updateWithKey",
           Ty "(Data.StringMap.Types.Key -> a -> Maybe a)      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "adjust",
           Ty "(a -> a)      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "adjustWithKey",
           Ty "(Data.StringMap.Types.Key -> a -> a)      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "intersection",
           Ty "Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "cutAllPx'",
           Ty "Data.StringMap.StringSet.StringSet      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "prefixFind",
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a -> [a]",
           A 2)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "toList",
           Ty "Data.StringMap.Base.StringMap a      -> [(Data.StringMap.Types.Key, a)]",
           A 1)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "prefixFindWithKey",
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> [(Data.StringMap.Types.Key, a)]",
           A 2)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "tree",
           Ty "Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 1)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "singleton",
           Ty "Data.StringMap.Types.Key -> a -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "insertWith",
           Ty "(a -> a -> a)      -> Data.StringMap.Types.Key      -> a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 4)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "insertWithKey",
           Ty "(Data.StringMap.Types.Key -> a -> a -> a)      -> Data.StringMap.Types.Key      -> a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 4)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "fromList",
           Ty "[(Data.StringMap.Types.Key, a)]      -> Data.StringMap.Base.StringMap a",
           A 1)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "prefixFindWithKeyBF",
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> [(Data.StringMap.Types.Key, a)]",
           A 2)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Strict",
           N "singleton",
           Ty "Data.StringMap.Types.Key -> a -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Strict",
           N "insertWith",
           Ty "(a -> a -> a)      -> Data.StringMap.Types.Key      -> a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 4)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Strict",
           N "insert",
           Ty "Data.StringMap.Types.Key      -> a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Strict",
           N "fromList",
           Ty "[(Data.StringMap.Types.Key, a)]      -> Data.StringMap.Base.StringMap a",
           A 1)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Strict",
           N "insertWithKey",
           Ty "(Data.StringMap.Types.Key -> a -> a -> a)      -> Data.StringMap.Types.Key      -> a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 4)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Strict",
           N "updateWithKey",
           Ty "(Data.StringMap.Types.Key -> a -> Maybe a)      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Strict",
           N "adjust",
           Ty "(a -> a)      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Strict",
           N "adjustWithKey",
           Ty "(Data.StringMap.Types.Key -> a -> a)      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Dim2Search",
           N "lookupRange",
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      ],
    C [
        E (Pkg "data-stringmap",
           Mod "Data.StringMap.StringSet",
           N "elemsPS",
           Ty "Data.StringMap.StringSet.StringSet -> [Data.StringMap.Types.Key]",
           A 1)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.StringSet",
           N "nullPS",
           Ty "Data.StringMap.StringSet.StringSet -> Bool",
           A 1)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "empty",
           Ty "Data.StringMap.Base.StringMap v",
           A 0)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "unNorm",
           Ty "Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 1)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "deepUnNorm",
           Ty "Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 1)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "!",
           Ty "Data.StringMap.Base.StringMap a -> Data.StringMap.Types.Key -> a",
           A 2)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "update",
           Ty "(a -> Maybe a)      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "delete",
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "union",
           Ty "Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "unionWith",
           Ty "(a -> a -> a)      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "unionWithKey",
           Ty "(Data.StringMap.Types.Key -> a -> a -> a)      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "cutPx'",
           Ty "Data.StringMap.StringSet.StringSet      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "elems",
           Ty "Data.StringMap.Base.StringMap a -> [a]",
           A 1)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "child",
           Ty "Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 1)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "toMap",
           Ty "Data.StringMap.Base.StringMap a      -> containers-0.5.6.2:Data.Map.Base.Map Data.StringMap.Types.Key a",
           A 1)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "fromKey",
           Ty "Data.StringMap.Types.Key -> Data.StringMap.Base.Key1",
           A 1)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "insert",
           Ty "Data.StringMap.Types.Key      -> a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "fromMap",
           Ty "containers-0.5.6.2:Data.Map.Base.Map Data.StringMap.Types.Key a      -> Data.StringMap.Base.StringMap a",
           A 1)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.FuzzySearch",
           N "lookupNoCase",
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.FuzzySearch",
           N "prefixFilterNoCase",
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.FuzzySearch",
           N "prefixFilter",
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Strict",
           N "update",
           Ty "(a -> Maybe a)      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Strict",
           N "delete",
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Strict",
           N "union",
           Ty "Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Strict",
           N "unionWith",
           Ty "(a -> a -> a)      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Dim2Search",
           N "lookupLE",
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Dim2Search",
           N "lookupGE",
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      ]
    , C [
        E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "normError'",
           Ty "String -> String -> a",
           A 2)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "toKey",
           Ty "Data.StringMap.Base.Key1 -> Data.StringMap.Types.Key",
           A 1)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "branch",
           Ty "Data.StringMap.Types.Sym      -> Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 3)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "norm",
           Ty "Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 1)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "lookupGE",
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "lookupLE",
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "toListShortestFirst",
           Ty "Data.StringMap.Base.StringMap v      -> [(Data.StringMap.Types.Key, v)]",
           A 1)
      ]
    , C [
        E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "siseq",
           Ty "Data.StringMap.Base.Key1      -> Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 2)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "deepNorm",
           Ty "Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 1)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "next",
           Ty "Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 1)
      , E (Pkg "data-stringmap",
           Mod "Data.StringMap.Base",
           N "value'",
           Ty "Data.StringMap.Base.StringMap v -> v",
           A 1)
      ]
    ]
