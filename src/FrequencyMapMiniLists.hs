module FrequencyMapMiniLists where

import Data.List.Split (chunksOf)
import qualified Data.Map as Map

import FrequencyMap (fromMap, frequencyMap)

type Count a = (Int, a)
type FrequencyCount a = [Count a]
type FrequencyMap a = Map.Map a Int

frequency :: Ord a => [a] -> FrequencyCount a
frequency = fromMap . fold . frequencyMapMiniLists

frequencyMapMiniLists :: Ord a => [a] -> [FrequencyMap a]
frequencyMapMiniLists as = map frequencyMap (splitList as)

fold :: Ord a => [FrequencyMap a] -> FrequencyMap a
fold = foldl (Map.unionWith (+)) Map.empty

splitList :: [a] -> [[a]]
splitList as = chunksOf 50 as
