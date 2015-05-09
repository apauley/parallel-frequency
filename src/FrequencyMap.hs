module FrequencyMap where

import Data.List
import Data.Tuple
import qualified Data.Map as Map

import Shared hiding (frequency)

type FrequencyMap a = Map.Map a Int

frequency :: Ord a => [a] -> FrequencyCount a
frequency = fromMap . frequencyDiv

fromMap :: Ord a => FrequencyMap a -> FrequencyCount a
fromMap m = reverse . sort $ map swap (Map.toList m)

frequencyDiv :: Ord a => [a] -> FrequencyMap a
frequencyDiv = parFrequencyMap . splitList

parFrequencyMap :: Ord a => ([a], [a]) -> FrequencyMap a
parFrequencyMap (xs, ys) = Map.unionWith (+) m1 m2
  where m1 = frequencyMap xs
        m2 = frequencyMap ys

frequencyMap :: Ord a => [a] -> FrequencyMap a
frequencyMap xs = Map.fromListWith (+) [(x, 1) | x <- xs]

splitList :: [a] -> ([a], [a])
splitList as = splitAt (length as `div` 2) as
