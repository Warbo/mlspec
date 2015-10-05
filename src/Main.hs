module Main where

import MLSpec.Theory
import System.Environment

main = do stdin <- getContents
          env   <- getEnvironment
          let size = fmap read (lookup "SIZE" env)
          dirs  <- writeTheoriesFromClusters size stdin
          mapM (putStrLn . ("PROJECT " ++)) dirs
