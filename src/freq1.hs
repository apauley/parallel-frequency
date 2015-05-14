import System.Environment (getArgs)

import Shared
import FrequencyList

main :: IO ()
main = do
  t0           <- printT0
  [fileName]   <- getArgs
  fileContents <- readFile fileName

  let wordFreq = frequency (words fileContents)
      charFreq = frequency fileContents
  printTimeSince t0 "After freq return."

  putStrLn $ "\nTop 10 words in " ++ fileName ++ ":"
  putStrLn $ summary (take 10 wordFreq)
  printTimeSince t0 "After word frequency print."

  putStrLn $ "\nTop 10 characters in " ++ fileName ++ ":"
  putStrLn $ summary (take 10 charFreq)
  printTimeSince t0 "After char frequency print."
