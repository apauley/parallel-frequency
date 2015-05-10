import System.Environment (getArgs)
import Data.Time (getCurrentTime)
import System.Random

import Control.Exception
import Control.Parallel.Strategies
import Control.DeepSeq

import Shared (printTimeSince, printRandomNumFrequency, summary)
import FrequencyMapUsing

main :: IO ()
main = do
  [fileName] <- getArgs
  seed       <- newStdGen

  fileContents <- readFile fileName

  putStrLn "Calculate frequency of elements in a list."
  t0 <- getCurrentTime

  (wordFreq, charFreq) <- evaluate (parCount fileContents)
  printTimeSince t0 "After parCount return."

  printRandomNumFrequency seed frequency
  printTimeSince t0 "After num frequency print."

  putStrLn $ "\nTop 10 words in " ++ fileName ++ ":"
  putStrLn $ summary (take 10 wordFreq)
  printTimeSince t0 "After word frequency print."

  putStrLn $ "\nTop 10 characters in " ++ fileName ++ ":"
  putStrLn $ summary (take 10 charFreq)
  printTimeSince t0 "After char frequency print."

parCount :: String -> (FrequencyCount String, FrequencyCount Char)
parCount fileContents = runEval $ do
  w <- rpar $ force (frequency (words fileContents))
  c <- rpar $ force (frequency fileContents)
  return (w, c)
