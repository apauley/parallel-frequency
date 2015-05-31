module FrequencyMapUsing (frequencyChunked) where

import qualified Data.Map as Map

import Control.Parallel.Strategies
import Control.DeepSeq

import FrequencyMap (fromMap, frequencyMap)

type Count a = (a, Int)
type FrequencyCount a = [Count a]
type FrequencyMap a = Map.Map a Int

frequencyChunked :: (NFData a, Ord a) => [[a]] -> FrequencyCount a
frequencyChunked = fromMap . fold . frequencyMapChunked

frequencyMapChunked :: (NFData a, Ord a) => [[a]] -> [FrequencyMap a]
frequencyMapChunked xs = map frequencyMap xs `using` parList (rpar . force)

fold :: (NFData a, Ord a) => [FrequencyMap a] -> FrequencyMap a
fold = foldl (Map.unionWith (+)) Map.empty
