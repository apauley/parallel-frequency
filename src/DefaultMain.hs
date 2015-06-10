{-# LANGUAGE RankNTypes #-}

module DefaultMain where

import System.Environment (getArgs)
import Control.DeepSeq
import Shared

defaultMain :: (forall a. (Show a, Ord a, NFData a) => [a] -> FrequencyCount a) -> IO ()
defaultMain frequency = do
  t0           <- printT0
  [fileName]   <- getArgs
  fileContents <- readFile fileName

  let charFreq = frequency fileContents
  printTimeSince t0 "After freq return."

  putStrLn $ "\nTop 10 characters in " ++ fileName ++ ":"
  putStrLn $ summary (take 10 charFreq)
  printTimeSince t0 "After character frequency print."
