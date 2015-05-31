module FrequencyMapUsing (frequencyChunked) where

import Control.Parallel.Strategies
import Control.DeepSeq

import FrequencyMap (FrequencyMap, fromMap, frequencyMap)
import FrequencyMapChunked (fold)
import Shared (FrequencyCount)

frequencyChunked :: (NFData a, Ord a) => [[a]] -> FrequencyCount a
frequencyChunked = fromMap . fold . frequencyMapChunked

frequencyMapChunked :: (NFData a, Ord a) => [[a]] -> [FrequencyMap a]
frequencyMapChunked xs = map frequencyMap xs `using` parList (rpar . force)
