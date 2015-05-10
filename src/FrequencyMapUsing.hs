module FrequencyMapUsing where

import Data.List.Split (chunksOf)
import qualified Data.Map as Map

import Control.Parallel.Strategies

import FrequencyMap (fromMap, frequencyMap)

type Count a = (Int, a)
type FrequencyCount a = [Count a]
type FrequencyMap a = Map.Map a Int

frequency :: (NFData a, Ord a) => [a] -> FrequencyCount a
frequency = fromMap . fold . frequencyMapMiniLists

frequencyMapMiniLists :: (NFData a, Ord a) => [a] -> [FrequencyMap a]
frequencyMapMiniLists as = map frequencyMap (split as) `using` parList rseq

fold :: (NFData a, Ord a) => [FrequencyMap a] -> FrequencyMap a
fold = foldl (Map.unionWith (+)) Map.empty

split :: [a] -> [[a]]
split = chunksOf 5000
