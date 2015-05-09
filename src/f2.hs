import System.Environment (getArgs)
import System.Random
import Data.Time (getCurrentTime)

import Shared

import Control.Parallel.Strategies

main :: IO ()
main = do
  [fileName] <- getArgs
  seed       <- newStdGen

  fileContents <- readFile fileName

  putStrLn "Calculate frequency of elements in a list."
  t0 <- getCurrentTime

  let numbers = take 1000000 $ randomIntStream seed
  let (wordFreq, charFreq, numFreq) = parFreq fileContents numbers
  printTimeSince t0 "after parFreq return"

  putStrLn $ "\nTop 10 words in " ++ fileName ++ ":"
  putStrLn wordFreq
  printTimeSince t0 "after word frequency"

  putStrLn $ "\nTop 10 characters in " ++ fileName ++ ":"
  putStrLn charFreq
  printTimeSince t0 "after char frequency"

  putStrLn $ "\nFrequency count for " ++ show (length numbers) ++ " random numbers:"
  putStrLn numFreq
  printTimeSince t0 "after num frequency"

randomIntStream :: StdGen -> [Int]
randomIntStream = randomRs (1,5)

parFreq :: String -> [Int] -> (String,  String, String)
parFreq fileContents numbers = runEval $ do
  w <- rpar (summary $ take 10 $ frequency (words fileContents))
  c <- rpar (summary $ take 10 $ frequency fileContents)
  n <- rpar (summary $ frequency numbers)
  return (w, c, n)
