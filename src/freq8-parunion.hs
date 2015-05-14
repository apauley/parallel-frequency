import System.Environment (getArgs)

import Control.Exception
import Control.Parallel.Strategies

import Shared (printTimeSince, summary, printT0)
import FrequencyMapParUnion

main :: IO ()
main = do
  t0           <- printT0
  [fileName]   <- getArgs
  fileContents <- readFile fileName

  (wordFreq, charFreq) <- evaluate (parCount fileContents)
  printTimeSince t0 "After parCount return."

  putStrLn $ "\nTop 10 words in " ++ fileName ++ ":"
  putStrLn $ summary (take 10 wordFreq)
  printTimeSince t0 "After word frequency print."

  putStrLn $ "\nTop 10 characters in " ++ fileName ++ ":"
  putStrLn $ summary (take 10 charFreq)
  printTimeSince t0 "After char frequency print."

parCount :: String -> (FrequencyCount String, FrequencyCount Char)
parCount fileContents = (w, c) `using` parTuple2 rpar rpar
  where w = frequency (words fileContents)
        c = frequency fileContents
