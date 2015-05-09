module FrequencyMap where

import Data.List
import Data.Tuple
import qualified Data.Map as Map

import Shared hiding (frequency)

frequency :: Ord a => [a] -> FrequencyCount a
frequency xs = reverse .sort $ map swap (Map.toList . frequencyMap $ xs)

frequencyMap :: Ord a => [a] -> Map.Map a Int
frequencyMap xs = Map.fromListWith (+) [(x, 1) | x <- xs]
