module FrequencyMap where

import Data.List
import Data.Tuple
import qualified Data.Map as Map

import Control.Parallel.Strategies
import Control.DeepSeq

import Shared hiding (frequency)

type FrequencyMap a = Map.Map a Int

frequency :: (NFData a, Ord a) => [a] -> FrequencyCount a
frequency = fromMap . frequencyDiv

fromMap :: (NFData a, Ord a) => FrequencyMap a -> FrequencyCount a
fromMap m = reverse . sort $ map swap (Map.toList m)

frequencyDiv :: (NFData a, Ord a) => [a] -> FrequencyMap a
frequencyDiv = parFrequencyMap . splitList

parFrequencyMap :: (NFData a, Ord a) => ([a], [a]) -> FrequencyMap a
parFrequencyMap = parFrequencyMap' 6

parFrequencyMap' :: (NFData a, Ord a) => Int -> ([a], [a]) -> FrequencyMap a
parFrequencyMap' 1 (xs, ys) = runEval $ do
  m1 <- rpar $ force (frequencyMap xs)
  m2 <- rpar $ force (frequencyMap ys)
  return $ Map.unionWith (+) m1 m2
parFrequencyMap' i (xs, ys) = Map.unionWith (+) m1 m2
  where m1 = parFrequencyMap' (i-1) (splitList xs)
        m2 = parFrequencyMap' (i-1) (splitList ys)

frequencyMap :: Ord a => [a] -> FrequencyMap a
frequencyMap xs = Map.fromListWith (+) [(x, 1) | x <- xs]

splitList :: [a] -> ([a], [a])
splitList as = splitAt (length as `div` 2) as
