import System.Environment (getArgs)
import Data.Time (getCurrentTime)

import Shared
import FrequencyMap

main :: IO ()
main = do
  [fileName]   <- getArgs
  fileContents <- readFile fileName

  putStrLn "Calculate frequency of elements in a list."
  t0 <- getCurrentTime

  let wordFreq = frequency (words fileContents)
      charFreq = frequency fileContents
  printTimeSince t0 "After freq return."

  putStrLn $ "\nTop 10 words in " ++ fileName ++ ":"
  putStrLn $ summary (take 10 wordFreq)
  printTimeSince t0 "After word frequency print."

  putStrLn $ "\nTop 10 characters in " ++ fileName ++ ":"
  putStrLn $ summary (take 10 charFreq)
  printTimeSince t0 "After char frequency print."
