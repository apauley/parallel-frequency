import System.Environment (getArgs)
import Data.Time (getCurrentTime)

import Control.Exception
import Control.Parallel.Strategies
import Control.DeepSeq

import Shared

main :: IO ()
main = do
  [fileName] <- getArgs

  fileContents <- readFile fileName

  putStrLn "Calculate frequency of elements in a list."
  t0 <- getCurrentTime

  (wordFreq, charFreq) <- evaluate (parFreq fileContents)
  printTimeSince t0 "after parFreq return"

  putStrLn $ "\nTop 10 words in " ++ fileName ++ ":"
  putStrLn wordFreq
  printTimeSince t0 "after word frequency"

  putStrLn $ "\nTop 10 characters in " ++ fileName ++ ":"
  putStrLn charFreq
  printTimeSince t0 "after char frequency"

parFreq :: String -> (String,  String)
parFreq fileContents = runEval $ do
  w <- rpar $ force (summary $ take 10 $ frequency (words fileContents))
  c <- rpar $ force (summary $ take 10 $ frequency fileContents)
  return (w, c)
