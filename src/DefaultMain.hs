module DefaultMain where

import System.Environment (getArgs)
import Shared

defaultMain :: (Show a, Ord a) => ([String] -> FrequencyCount a) -> IO ()
defaultMain frequency = do
  t0           <- printT0
  [fileName]   <- getArgs
  fileContents <- readFile fileName

  let wordFreq = frequency (words fileContents)
  printTimeSince t0 "After freq return."

  putStrLn $ "\nTop 10 words in " ++ fileName ++ ":"
  putStrLn $ summary (take 10 wordFreq)
  printTimeSince t0 "After word frequency print."
