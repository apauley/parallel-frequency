import System.Environment (getArgs)

import Control.Exception
import Control.Parallel.Strategies
import Control.DeepSeq

import Shared
import FrequencyList

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
parCount fileContents = runEval $ do
  w <- rpar $ force (frequency (words fileContents))
  c <- rpar $ force (frequency fileContents)
  return (w, c)
