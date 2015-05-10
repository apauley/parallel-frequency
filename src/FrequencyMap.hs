module FrequencyMap where

import qualified Data.Map as Map

import Shared (sortCount, FrequencyCount)

type FrequencyMap a = Map.Map a Int

frequency :: Ord a => [a] -> FrequencyCount a
frequency = fromMap . frequencyMap

fromMap :: Ord a => FrequencyMap a -> FrequencyCount a
fromMap m = sortCount $ Map.toList m

frequencyMap :: Ord a => [a] -> FrequencyMap a
frequencyMap xs = Map.fromListWith (+) [(x, 1) | x <- xs]
