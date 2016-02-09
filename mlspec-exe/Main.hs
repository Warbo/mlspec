module Main where

import Data.Text    as T
import Data.Text.IO as TIO
import MLSpec.Theory
import System.IO

main = do contents <- TIO.getContents
          outs     <- runTheoriesFromClusters (T.unpack contents)
          mapM (TIO.putStrLn . T.pack) outs
