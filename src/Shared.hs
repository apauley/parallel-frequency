module Shared where

import Text.Printf
import Data.Time.Clock
import Data.List

type Count a = (a, Int)
type FrequencyCount a = [Count a]

summary :: Show a => FrequencyCount a -> String
summary = foldl countStr ""

countStr :: Show a => String -> Count a -> String
countStr acc (a, count) = acc ++ show a ++ ":\t" ++ show count ++ "\n"

printT0 :: IO UTCTime
printT0 = do
  putStrLn "T0: start calculating frequency of elements in a list."
  t0 <- getCurrentTime
  return t0

printTimeSince :: UTCTime -> String -> IO ()
printTimeSince t0 desc = do
  t1 <- getCurrentTime
  putStr desc
  printf " Time since start of program: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)

sortCount :: (Ord a) => FrequencyCount a -> FrequencyCount a
sortCount = sortBy cmpCount

cmpCount :: Count a -> Count a -> Ordering
cmpCount (_, b1) (_, b2) = compare b2 b1
