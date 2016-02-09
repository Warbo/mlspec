module Main where

import MLSpec.Theory
import System.Environment
import System.IO

main = do args <- getArgs
          contents <- case args of
            []    -> getContents
            (f:_) -> readFile f
          outs     <- runTheoriesFromClusters contents
          mapM putStrLn outs
