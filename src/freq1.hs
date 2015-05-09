import System.Environment (getArgs)
import System.Random
import Data.Time (getCurrentTime)

import Shared

main :: IO ()
main = do
  [fileName] <- getArgs
  seed       <- newStdGen

  fileContents <- readFile fileName

  putStrLn "Calculate frequency of elements in a list."
  t0 <- getCurrentTime

  putStrLn $ "\nTop 10 words in " ++ fileName ++ ":"
  putStrLn $ summary $ take 10 $ frequency (words fileContents)
  printTimeSince t0 "after word frequency"

  putStrLn $ "\nTop 10 characters in " ++ fileName ++ ":"
  putStrLn $ summary $ take 10 $ frequency fileContents
  printTimeSince t0 "after char frequency"

  let numbers = take 1000000 $ randomIntStream seed
  putStrLn $ "\nFrequency count for " ++ show (length numbers) ++ " random numbers:"
  putStrLn $ summary $ frequency numbers
  printTimeSince t0 "after num frequency"

randomIntStream :: StdGen -> [Int]
randomIntStream = randomRs (1,5)
