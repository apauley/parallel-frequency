module FrequencyMapChunkedlist where

import Data.List.Split (chunksOf)
import qualified Data.Map as Map

import FrequencyMap (fromMap, frequencyMap)

type Count a = (a, Int)
type FrequencyCount a = [Count a]
type FrequencyMap a = Map.Map a Int

frequency :: Ord a => [a] -> FrequencyCount a
frequency = fromMap . fold . frequencyMapChunkedlist

frequencyMapChunkedlist :: Ord a => [a] -> [FrequencyMap a]
frequencyMapChunkedlist as = map frequencyMap (split as)

fold :: Ord a => [FrequencyMap a] -> FrequencyMap a
fold = foldl (Map.unionWith (+)) Map.empty

split :: [a] -> [[a]]
split = chunksOf 5000
