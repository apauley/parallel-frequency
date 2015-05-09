import System.Environment (getArgs)
import Data.Time (getCurrentTime)
import System.Random

import Control.Exception
import Control.Parallel.Strategies

import Shared

main :: IO ()
main = do
  [fileName] <- getArgs
  seed       <- newStdGen

  fileContents <- readFile fileName

  putStrLn "Calculate frequency of elements in a list."
  t0 <- getCurrentTime

  (wordFreq, charFreq) <- evaluate (parFreq fileContents)
  printTimeSince t0 "After parFreq return."

  printRandomNumFrequency seed
  printTimeSince t0 "After num frequency print."

  putStrLn $ "\nTop 10 words in " ++ fileName ++ ":"
  putStrLn $ summary (take 10 wordFreq)
  printTimeSince t0 "After word frequency print."

  putStrLn $ "\nTop 10 characters in " ++ fileName ++ ":"
  putStrLn $ summary (take 10 charFreq)
  printTimeSince t0 "After char frequency print."

parFreq :: String -> (FrequencyCount String, FrequencyCount Char)
parFreq fileContents = runEval $ do
  w <- rpar (frequency (words fileContents))
  c <- rpar (frequency fileContents)
  return (w, c)
