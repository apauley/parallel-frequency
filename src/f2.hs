import System.Environment (getArgs)
import System.Random

import Shared

import Control.Parallel.Strategies

main :: IO ()
main = do
  [fileName] <- getArgs
  seed       <- newStdGen

  fileContents <- readFile fileName

  let numbers = take 1000000 $ randomIntStream seed
  let (wordFreq, charFreq, numFreq) = parFreq fileContents numbers

  putStrLn $ "Top 10 words in " ++ fileName ++ ":"
  putStrLn wordFreq

  putStrLn $ "Top 10 characters in " ++ fileName ++ ":"
  putStrLn charFreq

  putStrLn $ "Frequency count for " ++ show (length numbers) ++ " random numbers:"
  putStrLn numFreq

randomIntStream :: StdGen -> [Int]
randomIntStream = randomRs (1,5)

parFreq :: String -> [Int] -> (String,  String, String)
parFreq fileContents numbers = runEval $ do
  w <- rpar (summary $ take 10 $ frequency (words fileContents))
  c <- rpar (summary $ take 10 $ frequency fileContents)
  n <- rpar (summary $ frequency numbers)
  return (w, c, n)
