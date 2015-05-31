module FrequencyMapChunked (frequencyChunked, fold) where

import qualified Data.Map as Map

import FrequencyMap (fromMap, frequencyMap)
import Shared (FrequencyCount)
import FrequencyMap (FrequencyMap)

frequencyChunked :: Ord a => [[a]] -> FrequencyCount a
frequencyChunked = fromMap . fold . frequencyMapChunked

frequencyMapChunked :: Ord a => [[a]] -> [FrequencyMap a]
frequencyMapChunked = map frequencyMap

fold :: Ord a => [FrequencyMap a] -> FrequencyMap a
fold = foldl (Map.unionWith (+)) Map.empty
