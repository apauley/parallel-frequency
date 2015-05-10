module Shared where

import Text.Printf
import Data.Time.Clock
import System.Random

import FrequencyList (FrequencyCount, Count)

summary :: Show a => FrequencyCount a -> String
summary freq = counters ++ "\n" ++ total
  where counters = foldl countStr "" freq
        total    = "Total: " ++ show (foldl (+) 0 $ map fst freq)

countStr :: Show a => String -> Count a -> String
countStr acc (count, a) = acc ++ show a ++ ":\t" ++ show count ++ "\n"

printTimeSince :: UTCTime -> String -> IO ()
printTimeSince t0 desc = do
  t1 <- getCurrentTime
  putStr desc
  printf " Time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)

printRandomNumFrequency :: StdGen -> ([Int] -> FrequencyCount Int) -> IO ()
printRandomNumFrequency seed freqFun = do
  let numbers = take 500000 $ randomIntStream seed
  putStrLn $ "\nFrequency count for " ++ show (length numbers) ++ " random numbers:"
  putStrLn $ summary $ freqFun numbers

randomIntStream :: StdGen -> [Int]
randomIntStream = randomRs (1,5)
