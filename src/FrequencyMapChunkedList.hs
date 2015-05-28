module FrequencyMapChunkedList (frequency) where

import qualified Data.Map as Map

import FrequencyMap (fromMap, frequencyMap)
import Shared (split)

type Count a = (a, Int)
type FrequencyCount a = [Count a]
type FrequencyMap a = Map.Map a Int

frequency :: Ord a => [a] -> FrequencyCount a
frequency = fromMap . fold . frequencyMapChunkedlist

frequencyMapChunkedlist :: Ord a => [a] -> [FrequencyMap a]
frequencyMapChunkedlist as = map frequencyMap (split as)

fold :: Ord a => [FrequencyMap a] -> FrequencyMap a
fold = foldl (Map.unionWith (+)) Map.empty
