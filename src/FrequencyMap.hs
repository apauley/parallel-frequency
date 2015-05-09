module FrequencyMap where

import Data.List
import Data.Tuple
import qualified Data.Map as Map

import Shared hiding (frequency)

type FrequencyMap a = Map.Map a Int

frequency :: Ord a => [a] -> FrequencyCount a
frequency = fromMap . frequencyMap

fromMap :: Ord a => FrequencyMap a -> FrequencyCount a
fromMap m = reverse . sort $ map swap (Map.toList m)

frequencyMap :: Ord a => [a] -> FrequencyMap a
frequencyMap xs = Map.fromListWith (+) [(x, 1) | x <- xs]

frequencyDiv :: Ord a => [a] -> FrequencyCount a
frequencyDiv as =
  let
    (xs,ys) = splitList as
    m1 = frequencyMap xs
    m2 = frequencyMap ys
  in
   fromMap $ Map.unionWith (+) m1 m2

splitList :: [a] -> ([a], [a])
splitList as = splitAt (length as `div` 2) as
