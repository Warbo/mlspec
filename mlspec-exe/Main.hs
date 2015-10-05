module Main where

import MLSpec.Theory

main = do stdin <- getContents
          outs  <- runTheoriesFromClusters stdin
          mapM putStrLn outs
