{-# LANGUAGE BangPatterns #-}

module FrequencyMapDivSplit (frequency) where

import qualified Data.Map.Strict as Map

import Control.Parallel.Strategies
import Control.DeepSeq

import FrequencyMap (FrequencyMap, fromMap, frequencyMap)
import Shared (FrequencyCount)

frequency :: (NFData a, Ord a) => [a] -> FrequencyCount a
frequency = fromMap . frequencyDiv

frequencyDiv :: (NFData a, Ord a) => [a] -> FrequencyMap a
frequencyDiv = parFrequencyMap . split

parFrequencyMap :: (NFData a, Ord a) => ([a], [a]) -> FrequencyMap a
parFrequencyMap = parFrequencyMap' 2

parFrequencyMap' :: (NFData a, Ord a) => Int -> ([a], [a]) -> FrequencyMap a
parFrequencyMap' 1 (!xs, !ys) = runEval $ do
  m1 <- rpar $ force (frequencyMap xs)
  m2 <- rpar $ force (frequencyMap ys)
  return $ Map.unionWith (+) m1 m2
parFrequencyMap' i (xs, ys) = Map.unionWith (+) m1 m2
  where m1 = parFrequencyMap' (i-1) (split xs)
        m2 = parFrequencyMap' (i-1) (split ys)

split :: [a] -> ([a], [a])
split as = splitAt (length as `div` 2) as
