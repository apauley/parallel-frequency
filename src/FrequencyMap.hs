module FrequencyMap where

import Data.List
import Data.Tuple
import qualified Data.Map as Map

type Count a = (Int, a)
type FrequencyCount a = [Count a]
type FrequencyMap a = Map.Map a Int

frequency :: Ord a => [a] -> FrequencyCount a
frequency = fromMap . frequencyMap

fromMap :: Ord a => FrequencyMap a -> FrequencyCount a
fromMap m = reverse . sort $ map swap (Map.toList m)

frequencyMap :: Ord a => [a] -> FrequencyMap a
frequencyMap xs = Map.fromListWith (+) [(x, 1) | x <- xs]
