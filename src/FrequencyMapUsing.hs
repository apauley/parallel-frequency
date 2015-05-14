module FrequencyMapUsing where

import qualified Data.Map as Map

import Control.Parallel.Strategies
import Control.DeepSeq

import FrequencyMap (fromMap, frequencyMap)
import Shared (split)

type Count a = (a, Int)
type FrequencyCount a = [Count a]
type FrequencyMap a = Map.Map a Int

frequency :: (NFData a, Ord a) => [a] -> FrequencyCount a
frequency = fromMap . fold . frequencyMapChunkedlist

frequencyMapChunkedlist :: (NFData a, Ord a) => [a] -> [FrequencyMap a]
frequencyMapChunkedlist as = map frequencyMap (split as) `using` parList (rpar . force)

fold :: (NFData a, Ord a) => [FrequencyMap a] -> FrequencyMap a
fold = foldl (Map.unionWith (+)) Map.empty
