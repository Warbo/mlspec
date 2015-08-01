module Main where

import MLSpec.Theory
import System.Environment

main = do stdin <- getContents
          [dir] <- getArgs
          env   <- getEnvironment
          let size = fmap read (lookup "SIZE" env)
          dirs  <- writeTheoriesFromClusters size dir stdin
          mapM (putStrLn . ("PROJECT " ++)) dirs
