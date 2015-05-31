module FrequencyParMap (frequencyChunked) where

import Control.Parallel.Strategies hiding (parMap)
import Control.DeepSeq

import FrequencyMap (FrequencyMap, fromMap, frequencyMap)
import FrequencyMapChunked (fold)
import Shared (FrequencyCount)

frequencyChunked :: (NFData a, Ord a) => [[a]] -> FrequencyCount a
frequencyChunked = fromMap . fold . frequencyMapChunked

frequencyMapChunked :: (NFData a, Ord a) => [[a]] -> [FrequencyMap a]
frequencyMapChunked xs = runEval $ parMap frequencyMap xs

parMap :: NFData b => (a -> b) -> [a] -> Eval [b]
parMap _ [] = return []
parMap f (a:as) = do
   b <- rpar $ force (f a)
   bs <- parMap f as
   return (b:bs)
