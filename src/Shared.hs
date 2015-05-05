module Shared where

type Count a = (Int, a)
type FrequencyCount a = [Count a]

summary :: Show a => FrequencyCount a -> String
summary freq = counters ++ "\n" ++ total
  where counters = foldl countStr "" freq
        total    = "Total: " ++ show (foldl (+) 0 $ map fst freq) ++ "\n"

countStr :: Show a => String -> Count a -> String
countStr acc (count, a) = acc ++ show a ++ ":\t" ++ show count ++ "\n"
