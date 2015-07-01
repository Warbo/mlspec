module Main where

import MLSpec.Theory
import System.Environment

main = do stdin <- getContents
          [dir] <- getArgs
          dirs  <- writeTheoriesFromClusters dir stdin
          mapM (putStrLn . ("PROJECT " ++)) dirs
