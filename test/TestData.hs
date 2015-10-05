module TestData where

import Language.Eval
import MLSpec.Theory

clusters = [
    C [
        E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.StringSet"],
           "foldPS"),
           Ty "(Data.StringMap.Types.Key -> b -> b)      -> b      -> (Data.StringMap.Types.Key -> Data.StringMap.Types.Key)      -> Data.StringMap.StringSet.StringSet      -> b",
           A 4)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.StringSet"],
           "foldWithKeyPS"),
           Ty "(Data.StringMap.Types.Key -> b -> b)      -> b -> Data.StringMap.StringSet.StringSet -> b",
           A 3)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "val"),
           Ty "v      -> Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 2)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "valueWithDefault"),
           Ty "a -> Data.StringMap.Base.StringMap a -> a",
           A 2)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "findWithDefault"),
           Ty "a      -> Data.StringMap.Types.Key -> Data.StringMap.Base.StringMap a -> a",
           A 3)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "lookupRange"),
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "updateWithKey"),
           Ty "(Data.StringMap.Types.Key -> a -> Maybe a)      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "adjust"),
           Ty "(a -> a)      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "adjustWithKey"),
           Ty "(Data.StringMap.Types.Key -> a -> a)      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "intersection"),
           Ty "Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "cutAllPx'"),
           Ty "Data.StringMap.StringSet.StringSet      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "prefixFind"),
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a -> [a]",
           A 2)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "toList"),
           Ty "Data.StringMap.Base.StringMap a      -> [(Data.StringMap.Types.Key, a)]",
           A 1)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "prefixFindWithKey"),
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> [(Data.StringMap.Types.Key, a)]",
           A 2)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "tree"),
           Ty "Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 1)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "singleton"),
           Ty "Data.StringMap.Types.Key -> a -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "insertWith"),
           Ty "(a -> a -> a)      -> Data.StringMap.Types.Key      -> a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 4)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "insertWithKey"),
           Ty "(Data.StringMap.Types.Key -> a -> a -> a)      -> Data.StringMap.Types.Key      -> a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 4)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "fromList"),
           Ty "[(Data.StringMap.Types.Key, a)]      -> Data.StringMap.Base.StringMap a",
           A 1)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "prefixFindWithKeyBF"),
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> [(Data.StringMap.Types.Key, a)]",
           A 2)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Strict"],
           "singleton"),
           Ty "Data.StringMap.Types.Key -> a -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Strict"],
           "insertWith"),
           Ty "(a -> a -> a)      -> Data.StringMap.Types.Key      -> a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 4)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Strict"],
           "insert"),
           Ty "Data.StringMap.Types.Key      -> a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Strict"],
           "fromList"),
           Ty "[(Data.StringMap.Types.Key, a)]      -> Data.StringMap.Base.StringMap a",
           A 1)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Strict"],
           "insertWithKey"),
           Ty "(Data.StringMap.Types.Key -> a -> a -> a)      -> Data.StringMap.Types.Key      -> a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 4)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Strict"],
           "updateWithKey"),
           Ty "(Data.StringMap.Types.Key -> a -> Maybe a)      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Strict"],
           "adjust"),
           Ty "(a -> a)      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Strict"],
           "adjustWithKey"),
           Ty "(Data.StringMap.Types.Key -> a -> a)      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Dim2Search"],
           "lookupRange"),
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      ],
    C [
        E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.StringSet"],
           "elemsPS"),
           Ty "Data.StringMap.StringSet.StringSet -> [Data.StringMap.Types.Key]",
           A 1)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.StringSet"],
           "nullPS"),
           Ty "Data.StringMap.StringSet.StringSet -> Bool",
           A 1)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "empty"),
           Ty "Data.StringMap.Base.StringMap v",
           A 0)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "unNorm"),
           Ty "Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 1)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "deepUnNorm"),
           Ty "Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 1)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "!"),
           Ty "Data.StringMap.Base.StringMap a -> Data.StringMap.Types.Key -> a",
           A 2)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "update"),
           Ty "(a -> Maybe a)      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "delete"),
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "union"),
           Ty "Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "unionWith"),
           Ty "(a -> a -> a)      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "unionWithKey"),
           Ty "(Data.StringMap.Types.Key -> a -> a -> a)      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "cutPx'"),
           Ty "Data.StringMap.StringSet.StringSet      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "elems"),
           Ty "Data.StringMap.Base.StringMap a -> [a]",
           A 1)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "child"),
           Ty "Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 1)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "toMap"),
           Ty "Data.StringMap.Base.StringMap a      -> containers-0.5.6.2:Data.Map.Base.Map Data.StringMap.Types.Key a",
           A 1)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "fromKey"),
           Ty "Data.StringMap.Types.Key -> Data.StringMap.Base.Key1",
           A 1)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "insert"),
           Ty "Data.StringMap.Types.Key      -> a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "fromMap"),
           Ty "containers-0.5.6.2:Data.Map.Base.Map Data.StringMap.Types.Key a      -> Data.StringMap.Base.StringMap a",
           A 1)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.FuzzySearch"],
           "lookupNoCase"),
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.FuzzySearch"],
           "prefixFilterNoCase"),
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.FuzzySearch"],
           "prefixFilter"),
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Strict"],
           "update"),
           Ty "(a -> Maybe a)      -> Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Strict"],
           "delete"),
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Strict"],
           "union"),
           Ty "Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Strict"],
           "unionWith"),
           Ty "(a -> a -> a)      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 3)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Dim2Search"],
           "lookupLE"),
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Dim2Search"],
           "lookupGE"),
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      ]
    , C [
        E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "normError'"),
           Ty "String -> String -> a",
           A 2)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "toKey"),
           Ty "Data.StringMap.Base.Key1 -> Data.StringMap.Types.Key",
           A 1)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "branch"),
           Ty "Data.StringMap.Types.Sym      -> Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 3)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "norm"),
           Ty "Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 1)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "lookupGE"),
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "lookupLE"),
           Ty "Data.StringMap.Types.Key      -> Data.StringMap.Base.StringMap a      -> Data.StringMap.Base.StringMap a",
           A 2)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "toListShortestFirst"),
           Ty "Data.StringMap.Base.StringMap v      -> [(Data.StringMap.Types.Key, v)]",
           A 1)
      ]
    , C [
        E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "siseq"),
           Ty "Data.StringMap.Base.Key1      -> Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 2)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "deepNorm"),
           Ty "Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 1)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "next"),
           Ty "Data.StringMap.Base.StringMap v      -> Data.StringMap.Base.StringMap v",
           A 1)
      , E (Expr ([Pkg "data-stringmap"],
           [Mod "Data.StringMap.Base"],
           "value'"),
           Ty "Data.StringMap.Base.StringMap v -> v",
           A 1)
      ]
    ]
