module Shared where

import Data.List
import Text.Printf
import Data.Time.Clock
import System.Random

type Count a = (Int, a)
type FrequencyCount a = [Count a]

frequency :: Ord a => [a] -> FrequencyCount a
frequency as = reverse . sort $ map (\a -> (length a, head a)) (group . sort $ as)

-- Prelude> let l = [4,5,6,4,5,6,7,7,2,3,5,1,2,3,4,5,4,3,4,3,6,2]
-- Prelude Data.List> group . sort $ l
-- [[1],[2,2,2],[3,3,3,3],[4,4,4,4,4],[5,5,5,5],[6,6,6],[7,7]]

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
