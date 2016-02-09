module Main where

import MLSpec.Theory
import System.IO

main = do hSetEncoding stdin  utf8
          hSetEncoding stdout utf8
          contents <- getContents
          outs  <- runTheoriesFromClusters contents
          mapM putStrLn outs
