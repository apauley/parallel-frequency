module DefaultMain where

import System.Environment (getArgs)
import Shared

defaultMain :: (Show a, Ord a) => ([Char] -> FrequencyCount a) -> IO ()
defaultMain frequency = do
  t0           <- printT0
  [fileName]   <- getArgs
  fileContents <- readFile fileName

  let charFreq = frequency fileContents
  printTimeSince t0 "After freq return."

  putStrLn $ "\nTop 10 characters in " ++ fileName ++ ":"
  putStrLn $ summary (take 10 charFreq)
  printTimeSince t0 "After character frequency print."
